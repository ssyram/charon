//! Trait resolution: given a trait reference, we track which local clause caused it to be true.
//! This module is independent from the rest of hax, in particular it doesn't use its
//! state-tracking machinery.

use itertools::Itertools;
use std::collections::{hash_map::Entry, HashMap};

use rustc_hir::def::DefKind;
use rustc_hir::def_id::DefId;
use rustc_middle::traits::CodegenObligationError;
use rustc_middle::ty::{self, *};
use rustc_trait_selection::traits::ImplSource;

use super::utils::{
    self, erase_and_norm, implied_predicates, normalize_bound_val, required_predicates,
    self_predicate, ToPolyTraitRef,
};

#[derive(Debug, Clone)]
pub enum PathChunk<'tcx> {
    AssocItem {
        item: AssocItem,
        /// The arguments provided to the item (for GATs). Includes trait args.
        generic_args: GenericArgsRef<'tcx>,
        /// The implemented predicate.
        predicate: PolyTraitPredicate<'tcx>,
        /// The index of this predicate in the list returned by `implied_predicates`.
        index: usize,
    },
    Parent {
        /// The implemented predicate.
        predicate: PolyTraitPredicate<'tcx>,
        /// The index of this predicate in the list returned by `implied_predicates`.
        index: usize,
    },
}
pub type Path<'tcx> = Vec<PathChunk<'tcx>>;

#[derive(Debug, Clone)]
pub enum ImplExprAtom<'tcx> {
    /// A concrete `impl Trait for Type {}` item.
    Concrete {
        def_id: DefId,
        generics: GenericArgsRef<'tcx>,
    },
    /// A context-bound clause like `where T: Trait`.
    LocalBound {
        predicate: Predicate<'tcx>,
        /// The nth (non-self) predicate found for this item. We use predicates from
        /// `required_predicates` starting from the parentmost item.
        index: usize,
        r#trait: PolyTraitRef<'tcx>,
        path: Path<'tcx>,
    },
    /// The automatic clause `Self: Trait` present inside a `impl Trait for Type {}` item.
    SelfImpl {
        r#trait: PolyTraitRef<'tcx>,
        path: Path<'tcx>,
    },
    /// `dyn Trait` is a wrapped value with a virtual table for trait
    /// `Trait`.  In other words, a value `dyn Trait` is a dependent
    /// triple that gathers a type τ, a value of type τ and an
    /// instance of type `Trait`.
    /// `dyn Trait` implements `Trait` using a built-in implementation; this refers to that
    /// built-in implementation.
    Dyn,
    /// A built-in trait whose implementation is computed by the compiler, such as `FnMut`. This
    /// morally points to an invisible `impl` block; as such it contains the information we may
    /// need from one.
    Builtin {
        /// Extra data for the given trait.
        trait_data: BuiltinTraitData<'tcx>,
        /// The `ImplExpr`s required to satisfy the implied predicates on the trait declaration.
        /// E.g. since `FnMut: FnOnce`, a built-in `T: FnMut` impl would have an `ImplExpr` for `T:
        /// FnOnce`.
        impl_exprs: Vec<ImplExpr<'tcx>>,
        /// The values of the associated types for this trait.
        types: Vec<(DefId, Ty<'tcx>, Vec<ImplExpr<'tcx>>)>,
    },
    /// An error happened while resolving traits.
    Error(String),
}

#[derive(Debug, Clone)]
pub enum BuiltinTraitData<'tcx> {
    /// A virtual `Drop` implementation.
    /// `Drop` doesn't work like a real trait but we want to pretend it does. If a type has a
    /// user-defined `impl Drop for X` we just use the `Concrete` variant, but if it doesn't we use
    /// this variant to supply the data needed to know what code will run on drop.
    Drop(DropData<'tcx>),
    /// Some other builtin trait.
    Other,
}

#[derive(Debug, Clone)]
pub enum DropData<'tcx> {
    /// A drop that does nothing, e.g. for scalars and pointers.
    Noop,
    /// An implicit `Drop` local clause, if the `resolve_drop_bounds` option is `false`. If that
    /// option is `true`, we'll add `Drop` bounds to every type param, and use that to resolve
    /// `Drop` impls of generics. If it's `false`, we use this variant to indicate that the drop
    /// clause comes from a generic or associated type.
    Implicit,
    /// The implicit `Drop` impl that exists for every type without an explicit `Drop` impl. The
    /// virtual impl is considered to have one `T: Drop` bound for each generic argument of the
    /// target type; it then simply drops each field in order.
    Glue {
        /// The type we're generating glue for.
        ty: Ty<'tcx>,
        /// The `ImplExpr`s for the `T: Drop` bounds of the virtual impl. There is one for each
        /// generic argument, in order.
        impl_exprs: Vec<ImplExpr<'tcx>>,
    },
}

#[derive(Clone, Debug)]
pub struct ImplExpr<'tcx> {
    /// The trait this is an impl for.
    pub r#trait: PolyTraitRef<'tcx>,
    /// The kind of implemention of the root of the tree.
    pub r#impl: ImplExprAtom<'tcx>,
}

/// Items have various predicates in scope. `path_to` uses them as a starting point for trait
/// resolution. This tracks where each of them comes from.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum BoundPredicateOrigin {
    /// The `Self: Trait` predicate implicitly present within trait declarations (note: we
    /// don't add it for trait implementations, should we?).
    SelfPred,
    /// The nth (non-self) predicate found for this item. We use predicates from
    /// `required_predicates` starting from the parentmost item.
    Item(usize),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct AnnotatedTraitPred<'tcx> {
    pub origin: BoundPredicateOrigin,
    pub clause: PolyTraitPredicate<'tcx>,
}

/// Returns the predicate to resolve as `Self`, if that makes sense in the current item.
/// Currently this predicate is only used inside trait declarations and their asosciated types.
fn initial_self_pred<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: rustc_span::def_id::DefId,
) -> Option<PolyTraitPredicate<'tcx>> {
    use DefKind::*;
    let trait_def_id = match tcx.def_kind(def_id) {
        Trait | TraitAlias => def_id,
        // Hack: we don't support GATs well so for now we let assoc types refer to the
        // implicit trait `Self` clause. Other associated items get an explicit `Self:
        // Trait` clause passed to them so they don't need that.
        AssocTy => tcx.parent(def_id),
        _ => return None,
    };
    let self_pred = self_predicate(tcx, trait_def_id).upcast(tcx);
    Some(self_pred)
}

/// The predicates to use as a starting point for resolving trait references within this item. This
/// includes the `required_predicates` of this item and all its parents.
fn local_bound_predicates<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: rustc_span::def_id::DefId,
    add_drop: bool,
) -> Vec<PolyTraitPredicate<'tcx>> {
    fn acc_predicates<'tcx>(
        tcx: TyCtxt<'tcx>,
        def_id: rustc_span::def_id::DefId,
        add_drop: bool,
        predicates: &mut Vec<PolyTraitPredicate<'tcx>>,
    ) {
        use DefKind::*;
        match tcx.def_kind(def_id) {
            // These inherit predicates from their parent.
            AssocTy | AssocFn | AssocConst | Closure | Ctor(..) | Variant => {
                let parent = tcx.parent(def_id);
                acc_predicates(tcx, parent, add_drop, predicates);
            }
            _ => {}
        }
        predicates.extend(
            required_predicates(tcx, def_id, add_drop)
                .iter()
                .map(|(clause, _span)| *clause)
                .filter_map(|clause| clause.as_trait_clause()),
        );
    }

    let mut predicates = vec![];
    acc_predicates(tcx, def_id, add_drop, &mut predicates);
    predicates
}

#[tracing::instrument(level = "trace", skip(tcx))]
fn parents_trait_predicates<'tcx>(
    tcx: TyCtxt<'tcx>,
    pred: PolyTraitPredicate<'tcx>,
    add_drop: bool,
) -> Vec<PolyTraitPredicate<'tcx>> {
    let self_trait_ref = pred.to_poly_trait_ref();
    implied_predicates(tcx, pred.def_id(), add_drop)
        .iter()
        .map(|(clause, _span)| *clause)
        // Substitute with the `self` args so that the clause makes sense in the
        // outside context.
        .map(|clause| clause.instantiate_supertrait(tcx, self_trait_ref))
        .filter_map(|pred| pred.as_trait_clause())
        .collect()
}

/// A candidate projects `self` along a path reaching some predicate. A candidate is
/// selected when its predicate is the one expected, aka `target`.
#[derive(Debug, Clone)]
struct Candidate<'tcx> {
    path: Path<'tcx>,
    pred: PolyTraitPredicate<'tcx>,
    origin: AnnotatedTraitPred<'tcx>,
}

impl<'tcx> Candidate<'tcx> {
    fn into_impl_expr(self, tcx: TyCtxt<'tcx>) -> ImplExprAtom<'tcx> {
        let path = self.path;
        let r#trait = self.origin.clause.to_poly_trait_ref();
        match self.origin.origin {
            BoundPredicateOrigin::SelfPred => ImplExprAtom::SelfImpl { r#trait, path },
            BoundPredicateOrigin::Item(index) => ImplExprAtom::LocalBound {
                predicate: self.origin.clause.upcast(tcx),
                index,
                r#trait,
                path,
            },
        }
    }
}

/// Stores a set of predicates along with where they came from.
#[derive(Clone)]
pub struct PredicateSearcher<'tcx> {
    tcx: TyCtxt<'tcx>,
    typing_env: rustc_middle::ty::TypingEnv<'tcx>,
    /// Local clauses available in the current context.
    candidates: HashMap<PolyTraitPredicate<'tcx>, Candidate<'tcx>>,
    /// Whether to add `T: Drop` bounds for every type generic and associated item.
    add_drop: bool,
    /// Count the number of bound clauses in scope; used to identify clauses uniquely.
    bound_clause_count: usize,
}

impl<'tcx> PredicateSearcher<'tcx> {
    /// Initialize the elaborator with the predicates accessible within this item.
    pub fn new_for_owner(tcx: TyCtxt<'tcx>, owner_id: DefId, add_drop: bool) -> Self {
        let mut out = Self {
            tcx,
            typing_env: TypingEnv {
                param_env: tcx.param_env(owner_id),
                typing_mode: TypingMode::PostAnalysis,
            },
            candidates: Default::default(),
            add_drop,
            bound_clause_count: 0,
        };
        out.insert_predicates(
            initial_self_pred(tcx, owner_id).map(|clause| AnnotatedTraitPred {
                origin: BoundPredicateOrigin::SelfPred,
                clause,
            }),
        );
        out.insert_bound_predicates(local_bound_predicates(tcx, owner_id, add_drop));
        out
    }

    /// Insert the bound clauses in the search context. Prefer inserting them all at once as this
    /// will give priority to shorter resolution paths. Bound clauses are numbered from `0` in
    /// insertion order.
    pub fn insert_bound_predicates(
        &mut self,
        clauses: impl IntoIterator<Item = PolyTraitPredicate<'tcx>>,
    ) {
        let mut count = usize::MAX;
        // Swap to avoid borrow conflicts.
        std::mem::swap(&mut count, &mut self.bound_clause_count);
        self.insert_predicates(clauses.into_iter().map(|clause| {
            let i = count;
            count += 1;
            AnnotatedTraitPred {
                origin: BoundPredicateOrigin::Item(i),
                clause,
            }
        }));
        std::mem::swap(&mut count, &mut self.bound_clause_count);
    }

    /// Override the param env; we use this when resolving `dyn` predicates to add more clauses to
    /// the scope.
    pub fn set_param_env(&mut self, param_env: ParamEnv<'tcx>) {
        self.typing_env.param_env = param_env;
    }

    /// Insert annotated predicates in the search context. Prefer inserting them all at once as
    /// this will give priority to shorter resolution paths.
    fn insert_predicates(&mut self, preds: impl IntoIterator<Item = AnnotatedTraitPred<'tcx>>) {
        self.insert_candidates(preds.into_iter().map(|clause| Candidate {
            path: vec![],
            pred: clause.clause,
            origin: clause,
        }))
    }

    /// Insert new candidates and all their parent predicates. This deduplicates predicates
    /// to avoid divergence.
    fn insert_candidates(&mut self, candidates: impl IntoIterator<Item = Candidate<'tcx>>) {
        let tcx = self.tcx;
        // Filter out duplicated candidates.
        let mut new_candidates = Vec::new();
        for mut candidate in candidates {
            // Normalize and erase all lifetimes.
            candidate.pred = normalize_bound_val(tcx, self.typing_env, candidate.pred);
            if let Entry::Vacant(entry) = self.candidates.entry(candidate.pred) {
                entry.insert(candidate.clone());
                new_candidates.push(candidate);
            }
        }
        if !new_candidates.is_empty() {
            // Insert the parents all at once.
            self.insert_candidate_parents(new_candidates);
        }
    }

    /// Add the parents of these candidates. This is a separate function to avoid
    /// polymorphic recursion due to the closures capturing the type parameters of this
    /// function.
    fn insert_candidate_parents(&mut self, new_candidates: Vec<Candidate<'tcx>>) {
        let tcx = self.tcx;
        // Then recursively add their parents. This way ensures a breadth-first order,
        // which means we select the shortest path when looking up predicates.
        let add_drop = self.add_drop;
        self.insert_candidates(new_candidates.into_iter().flat_map(|candidate| {
            parents_trait_predicates(tcx, candidate.pred, add_drop)
                .into_iter()
                .enumerate()
                .map(move |(index, parent_pred)| {
                    let mut parent_candidate = Candidate {
                        pred: parent_pred,
                        path: candidate.path.clone(),
                        origin: candidate.origin,
                    };
                    parent_candidate.path.push(PathChunk::Parent {
                        predicate: parent_pred,
                        index,
                    });
                    parent_candidate
                })
        }));
    }

    /// If the type is a trait associated type, we add any relevant bounds to our context.
    fn add_associated_type_refs(
        &mut self,
        ty: Binder<'tcx, Ty<'tcx>>,
        // Call back into hax-related code to display a nice warning.
        warn: &impl Fn(&str),
    ) -> Result<(), String> {
        let tcx = self.tcx;
        // Note: We skip a binder but rebind it just after.
        let TyKind::Alias(AliasTyKind::Projection, alias_ty) = ty.skip_binder().kind() else {
            return Ok(());
        };
        let trait_ref = ty.rebind(alias_ty.trait_ref(tcx)).upcast(tcx);

        // The predicate we're looking for is is `<T as Trait>::Type: OtherTrait`. We look up `T as
        // Trait` in the current context and add all the bounds on `Trait::Type` to our context.
        let Some(trait_candidate) = self.resolve_local(trait_ref, warn)? else {
            return Ok(());
        };

        // The bounds that hold on the associated type.
        let item_bounds = implied_predicates(tcx, alias_ty.def_id, self.add_drop);
        let item_bounds = item_bounds
            .iter()
            .map(|(clause, _span)| *clause)
            .filter_map(|pred| pred.as_trait_clause())
            // Substitute the item generics
            .map(|pred| EarlyBinder::bind(pred).instantiate(tcx, alias_ty.args))
            .enumerate();

        // Add all the bounds on the corresponding associated item.
        self.insert_candidates(item_bounds.map(|(index, pred)| {
            let mut candidate = Candidate {
                path: trait_candidate.path.clone(),
                pred,
                origin: trait_candidate.origin,
            };
            candidate.path.push(PathChunk::AssocItem {
                item: tcx.associated_item(alias_ty.def_id),
                generic_args: alias_ty.args,
                predicate: pred,
                index,
            });
            candidate
        }));

        Ok(())
    }

    /// Resolve a local clause by looking it up in this set. If the predicate applies to an
    /// associated type, we add the relevant implied associated type bounds to the set as well.
    fn resolve_local(
        &mut self,
        target: PolyTraitPredicate<'tcx>,
        // Call back into hax-related code to display a nice warning.
        warn: &impl Fn(&str),
    ) -> Result<Option<Candidate<'tcx>>, String> {
        tracing::trace!("Looking for {target:?}");

        // Look up the predicate
        let ret = self.candidates.get(&target).cloned();
        if ret.is_some() {
            return Ok(ret);
        }

        // Add clauses related to associated type in the `Self` type of the predicate.
        self.add_associated_type_refs(target.self_ty(), warn)?;

        let ret = self.candidates.get(&target).cloned();
        if ret.is_none() {
            tracing::trace!(
                "Couldn't find {target:?} in: [\n{}]",
                self.candidates
                    .iter()
                    .map(|(_, c)| format!("  - {:?}\n", c.pred))
                    .join("")
            );
        }
        Ok(ret)
    }

    /// Resolve the given trait reference in the local context.
    #[tracing::instrument(level = "trace", skip(self, warn))]
    pub fn resolve(
        &mut self,
        tref: &PolyTraitRef<'tcx>,
        // Call back into hax-related code to display a nice warning.
        warn: &impl Fn(&str),
    ) -> Result<ImplExpr<'tcx>, String> {
        use rustc_trait_selection::traits::{
            BuiltinImplSource, ImplSource, ImplSourceUserDefinedData,
        };
        let tcx = self.tcx;
        let drop_trait = tcx.lang_items().drop_trait().unwrap();

        let erased_tref = normalize_bound_val(self.tcx, self.typing_env, *tref);
        let trait_def_id = erased_tref.skip_binder().def_id;

        let error = |msg: String| {
            warn(&msg);
            Ok(ImplExpr {
                r#impl: ImplExprAtom::Error(msg),
                r#trait: *tref,
            })
        };

        let impl_source = shallow_resolve_trait_ref(tcx, self.typing_env.param_env, erased_tref);
        let atom = match impl_source {
            Ok(ImplSource::UserDefined(ImplSourceUserDefinedData {
                impl_def_id,
                args: generics,
                ..
            })) => ImplExprAtom::Concrete {
                def_id: impl_def_id,
                generics,
            },
            Ok(ImplSource::Param(_)) => {
                match self.resolve_local(erased_tref.upcast(self.tcx), warn)? {
                    Some(candidate) => candidate.into_impl_expr(tcx),
                    None => {
                        let msg = format!(
                            "Could not find a clause for `{tref:?}` in the item parameters"
                        );
                        return error(msg);
                    }
                }
            }
            Ok(ImplSource::Builtin(BuiltinImplSource::Object { .. }, _)) => ImplExprAtom::Dyn,
            Ok(ImplSource::Builtin(_, _)) => {
                // Resolve the predicates implied by the trait.
                // If we wanted to not skip this binder, we'd have to instantiate the bound
                // regions, solve, then wrap the result in a binder. And track higher-kinded
                // clauses better all over.
                let impl_exprs = self.resolve_item_implied_predicates(
                    trait_def_id,
                    erased_tref.skip_binder().args,
                    warn,
                )?;
                let types = tcx
                    .associated_items(trait_def_id)
                    .in_definition_order()
                    .filter(|assoc| matches!(assoc.kind, AssocKind::Type { .. }))
                    .filter_map(|assoc| {
                        let ty =
                            Ty::new_projection(tcx, assoc.def_id, erased_tref.skip_binder().args);
                        let ty = erase_and_norm(tcx, self.typing_env, ty);
                        if let TyKind::Alias(_, alias_ty) = ty.kind() {
                            if alias_ty.def_id == assoc.def_id {
                                // Couldn't normalize the type to anything different than itself;
                                // this must be a built-in associated type such as
                                // `DiscriminantKind::Discriminant`.
                                // We can't return the unnormalized associated type as that would
                                // make the trait ref contain itself, which would make hax's
                                // `sinto` infrastructure loop. That's ok because we can't provide
                                // a value for this type other than the associate type alias
                                // itself.
                                return None;
                            }
                        }
                        let impl_exprs = self
                            .resolve_item_implied_predicates(
                                assoc.def_id,
                                erased_tref.skip_binder().args,
                                warn,
                            )
                            .ok()?;
                        Some((assoc.def_id, ty, impl_exprs))
                    })
                    .collect();
                ImplExprAtom::Builtin {
                    trait_data: BuiltinTraitData::Other,
                    impl_exprs,
                    types,
                }
            }
            // Resolve `Drop` trait impls by adding virtual impls when a real one can't be found.
            Err(CodegenObligationError::Unimplemented)
                if erased_tref.skip_binder().def_id == drop_trait =>
            {
                // If we wanted to not skip this binder, we'd have to instantiate the bound
                // regions, solve, then wrap the result in a binder. And track higher-kinded
                // clauses better all over.
                let mut resolve_drop = |ty: Ty<'tcx>| {
                    let tref = ty::Binder::dummy(ty::TraitRef::new(tcx, drop_trait, [ty]));
                    self.resolve(&tref, warn)
                };
                let find_drop_impl = |ty: Ty<'tcx>| {
                    let mut dtor = None;
                    tcx.for_each_relevant_impl(drop_trait, ty, |impl_did| {
                        dtor = Some(impl_did);
                    });
                    dtor
                };
                // TODO: how to check if there is a real drop impl?????
                let ty = erased_tref.skip_binder().args[0].as_type().unwrap();
                // Source of truth are `ty::needs_drop_components` and `tcx.needs_drop_raw`.
                let drop_data = match ty.kind() {
                    // TODO: Does `UnsafeBinder` drop its contents?
                    ty::Bool
                    | ty::Char
                    | ty::Int(..)
                    | ty::Uint(..)
                    | ty::Float(..)
                    | ty::Foreign(..)
                    | ty::Str
                    | ty::RawPtr(..)
                    | ty::Ref(..)
                    | ty::FnDef(..)
                    | ty::FnPtr(..)
                    | ty::UnsafeBinder(..)
                    | ty::Never => Ok(DropData::Noop),
                    ty::Array(inner_ty, _) | ty::Pat(inner_ty, _) | ty::Slice(inner_ty) => {
                        Ok(DropData::Glue {
                            ty,
                            impl_exprs: vec![resolve_drop(*inner_ty)?],
                        })
                    }
                    ty::Tuple(tys) => Ok(DropData::Glue {
                        ty,
                        impl_exprs: tys.iter().map(resolve_drop).try_collect()?,
                    }),
                    ty::Adt(..) if let Some(_) = find_drop_impl(ty) => {
                        // We should have been able to resolve the `T: Drop` clause above, if we
                        // get here we don't know how to reconstruct the arguments to the impl.
                        let msg = format!("Cannot resolve clause `{tref:?}`");
                        return error(msg);
                    }
                    ty::Adt(_, args)
                    | ty::Closure(_, args)
                    | ty::Coroutine(_, args)
                    | ty::CoroutineClosure(_, args)
                    | ty::CoroutineWitness(_, args) => Ok(DropData::Glue {
                        ty,
                        impl_exprs: args
                            .iter()
                            .filter_map(|arg| arg.as_type())
                            .map(resolve_drop)
                            .try_collect()?,
                    }),
                    // Every `dyn` has a `Drop` in its vtable, ergo we pretend that every `dyn` has
                    // `Drop` in its list of traits.
                    ty::Dynamic(..) => Err(ImplExprAtom::Dyn),
                    ty::Param(..) | ty::Alias(..) | ty::Bound(..) => {
                        if self.add_drop {
                            // We've added `Drop` impls on everything, we should be able to resolve
                            // it.
                            match self.resolve_local(erased_tref.upcast(self.tcx), warn)? {
                                Some(candidate) => Err(candidate.into_impl_expr(tcx)),
                                None => {
                                    let msg =
                                        format!("Cannot find virtual `Drop` clause: `{tref:?}`");
                                    return error(msg);
                                }
                            }
                        } else {
                            Ok(DropData::Implicit)
                        }
                    }

                    ty::Placeholder(..) | ty::Infer(..) | ty::Error(..) => {
                        let msg = format!(
                            "Cannot resolve clause `{tref:?}` \
                                because of a type error"
                        );
                        return error(msg);
                    }
                };
                match drop_data {
                    Ok(drop_data) => {
                        let impl_exprs = self.resolve_item_implied_predicates(
                            trait_def_id,
                            erased_tref.skip_binder().args,
                            warn,
                        )?;
                        ImplExprAtom::Builtin {
                            trait_data: BuiltinTraitData::Drop(drop_data),
                            impl_exprs,
                            types: vec![],
                        }
                    }
                    Err(atom) => atom,
                }
            }
            Err(e) => {
                let msg = format!(
                    "Could not find a clause for `{tref:?}` \
                    in the current context: `{e:?}`"
                );
                return error(msg);
            }
        };

        Ok(ImplExpr {
            r#impl: atom,
            r#trait: *tref,
        })
    }

    /// Resolve the predicates required by the given item.
    pub fn resolve_item_required_predicates(
        &mut self,
        def_id: DefId,
        generics: GenericArgsRef<'tcx>,
        // Call back into hax-related code to display a nice warning.
        warn: &impl Fn(&str),
    ) -> Result<Vec<ImplExpr<'tcx>>, String> {
        let tcx = self.tcx;
        self.resolve_predicates(
            generics,
            required_predicates(tcx, def_id, self.add_drop),
            warn,
        )
    }

    /// Resolve the predicates implied by the given item.
    pub fn resolve_item_implied_predicates(
        &mut self,
        def_id: DefId,
        generics: GenericArgsRef<'tcx>,
        // Call back into hax-related code to display a nice warning.
        warn: &impl Fn(&str),
    ) -> Result<Vec<ImplExpr<'tcx>>, String> {
        let tcx = self.tcx;
        self.resolve_predicates(
            generics,
            implied_predicates(tcx, def_id, self.add_drop),
            warn,
        )
    }

    /// Apply the given generics to the provided clauses and resolve the trait references in the
    /// current context.
    pub fn resolve_predicates(
        &mut self,
        generics: GenericArgsRef<'tcx>,
        predicates: utils::Predicates<'tcx>,
        // Call back into hax-related code to display a nice warning.
        warn: &impl Fn(&str),
    ) -> Result<Vec<ImplExpr<'tcx>>, String> {
        let tcx = self.tcx;
        predicates
            .iter()
            .map(|(clause, _span)| *clause)
            .filter_map(|clause| clause.as_trait_clause())
            .map(|trait_pred| trait_pred.map_bound(|p| p.trait_ref))
            // Substitute the item generics
            .map(|trait_ref| EarlyBinder::bind(trait_ref).instantiate(tcx, generics))
            // Resolve
            .map(|trait_ref| self.resolve(&trait_ref, warn))
            .collect()
    }
}

/// Attempts to resolve an obligation to an `ImplSource`. The result is a shallow `ImplSource`
/// resolution, meaning that we do not resolve all nested obligations on the impl. Note that type
/// check should guarantee to us that all nested obligations *could be* resolved if we wanted to.
///
/// This expects that `trait_ref` is fully normalized.
///
/// This is based on `rustc_traits::codegen::codegen_select_candidate` in rustc.
pub fn shallow_resolve_trait_ref<'tcx>(
    tcx: TyCtxt<'tcx>,
    param_env: ParamEnv<'tcx>,
    trait_ref: PolyTraitRef<'tcx>,
) -> Result<ImplSource<'tcx, ()>, CodegenObligationError> {
    use rustc_infer::infer::TyCtxtInferExt;
    use rustc_middle::traits::CodegenObligationError;
    use rustc_middle::ty::TypeVisitableExt;
    use rustc_trait_selection::traits::{
        Obligation, ObligationCause, ObligationCtxt, SelectionContext, SelectionError,
    };
    // Do the initial selection for the obligation. This yields the
    // shallow result we are looking for -- that is, what specific impl.
    let infcx = tcx
        .infer_ctxt()
        .ignoring_regions()
        .build(TypingMode::PostAnalysis);
    let mut selcx = SelectionContext::new(&infcx);

    let obligation_cause = ObligationCause::dummy();
    let obligation = Obligation::new(tcx, obligation_cause, param_env, trait_ref);

    let selection = match selcx.poly_select(&obligation) {
        Ok(Some(selection)) => selection,
        Ok(None) => return Err(CodegenObligationError::Ambiguity),
        Err(SelectionError::Unimplemented) => return Err(CodegenObligationError::Unimplemented),
        Err(_) => return Err(CodegenObligationError::Ambiguity),
    };

    // Currently, we use a fulfillment context to completely resolve
    // all nested obligations. This is because they can inform the
    // inference of the impl's type parameters.
    // FIXME(-Znext-solver): Doesn't need diagnostics if new solver.
    let ocx = ObligationCtxt::new(&infcx);
    let impl_source = selection.map(|obligation| {
        ocx.register_obligation(obligation.clone());
        ()
    });

    let errors = ocx.select_all_or_error();
    if !errors.is_empty() {
        return Err(CodegenObligationError::Ambiguity);
    }

    let impl_source = infcx.resolve_vars_if_possible(impl_source);
    let impl_source = tcx.erase_regions(impl_source);

    if impl_source.has_infer() {
        // Unused lifetimes on an impl get replaced with inference vars, but never resolved.
        return Err(CodegenObligationError::Ambiguity);
    }

    Ok(impl_source)
}
