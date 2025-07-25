use super::translate_bodies::BodyTransCtx;
use super::translate_crate::*;
use super::translate_ctx::*;
use charon_lib::ast::*;
use charon_lib::formatter::IntoFormatter;
use charon_lib::pretty::FmtWithCtx;
use core::panic;
use derive_generic_visitor::Visitor;
use hax_frontend_exporter as hax;
use indexmap::IndexMap;
use itertools::Itertools;
use std::mem;
use std::ops::ControlFlow;
use std::sync::Arc;

/// To tell if the given `hax::TraitRef` is a parent trait reference.
pub fn is_hax_parent_trait_ref(tref: &hax::TraitRef) -> bool {
    // to see if the first type parameter of the Trait Ref is `Self`
    for arg in &tref.generic_args {
        match arg {
            hax::GenericArg::Type(ty) => match ty.kind() {
                // hax guarantees that `Self` is always having id 0
                hax::TyKind::Param(param_ty) => return param_ty.index == 0,
                // If the first type parameter is not even a type variable
                // Then it must not be a parent trait ref
                _ => return false,
            },
            _ => {}
        }
    }
    false
}

impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    pub(crate) fn translate_item(&mut self, item_src: &TransItemSource) {
        let trans_id = self.register_id_no_enqueue(&None, item_src);
        let def_id = item_src.as_def_id();
        self.with_def_id(def_id, trans_id, |mut ctx| {
            let span = ctx.def_span(def_id);
            // Catch cycles
            let res = {
                // Stopgap measure because there are still many panics in charon and hax.
                let mut ctx = std::panic::AssertUnwindSafe(&mut ctx);
                std::panic::catch_unwind(move || ctx.translate_item_aux(item_src, trans_id))
            };
            match res {
                Ok(Ok(())) => return,
                // Translation error
                Ok(Err(_)) => {
                    register_error!(ctx, span, "Item `{def_id:?}` caused errors; ignoring.")
                }
                // Panic
                Err(_) => register_error!(
                    ctx,
                    span,
                    "Thread panicked when extracting item `{def_id:?}`."
                ),
            };
        })
    }

    pub(crate) fn translate_item_aux(
        &mut self,
        item_src: &TransItemSource,
        trans_id: Option<AnyTransId>,
    ) -> Result<(), Error> {
        // Translate the meta information
        let name = self.translate_name(item_src)?;
        if let Some(trans_id) = trans_id {
            self.translated.item_names.insert(trans_id, name.clone());
        }
        let opacity = self.opacity_for_name(&name);
        if opacity.is_invisible() {
            // Don't even start translating the item. In particular don't call `hax_def` on it.
            return Ok(());
        }
        let def = self.hax_def(item_src.as_def_id())?;
        let item_meta = self.translate_item_meta(&def, item_src, name, opacity);

        // Initialize the item translation context
        let mut bt_ctx = ItemTransCtx::new(item_src.clone(), trans_id, self);
        match &item_src.kind {
            TransItemSourceKind::InherentImpl | TransItemSourceKind::Module => {
                bt_ctx.register_module(item_meta, &def);
            }
            TransItemSourceKind::Type => {
                let Some(AnyTransId::Type(id)) = trans_id else {
                    unreachable!()
                };
                let ty = bt_ctx.translate_type_decl(id, item_meta, &def)?;
                self.translated.type_decls.set_slot(id, ty);
            }
            TransItemSourceKind::Fun => {
                let Some(AnyTransId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_function(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::Global => {
                let Some(AnyTransId::Global(id)) = trans_id else {
                    unreachable!()
                };
                let global_decl = bt_ctx.translate_global(id, item_meta, &def)?;
                self.translated.global_decls.set_slot(id, global_decl);
            }
            TransItemSourceKind::TraitDecl => {
                let Some(AnyTransId::TraitDecl(id)) = trans_id else {
                    unreachable!()
                };
                let trait_decl = bt_ctx.translate_trait_decl(id, item_meta, &def)?;
                self.translated.trait_decls.set_slot(id, trait_decl);
            }
            TransItemSourceKind::TraitImpl => {
                let Some(AnyTransId::TraitImpl(id)) = trans_id else {
                    unreachable!()
                };
                let trait_impl = bt_ctx.translate_trait_impl(id, item_meta, &def)?;
                self.translated.trait_impls.set_slot(id, trait_impl);
            }
            TransItemSourceKind::ClosureTraitImpl(kind) => {
                let Some(AnyTransId::TraitImpl(id)) = trans_id else {
                    unreachable!()
                };
                let closure_trait_impl =
                    bt_ctx.translate_closure_trait_impl(id, item_meta, &def, *kind)?;
                self.translated.trait_impls.set_slot(id, closure_trait_impl);
            }
            TransItemSourceKind::ClosureMethod(kind) => {
                let Some(AnyTransId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_closure_method(id, item_meta, &def, *kind)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::ClosureAsFnCast => {
                let Some(AnyTransId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_stateless_closure_as_fn(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::DropGlueImpl => {
                let Some(AnyTransId::TraitImpl(id)) = trans_id else {
                    unreachable!()
                };
                let timpl = bt_ctx.translate_drop_impl(id, item_meta, &def)?;
                self.translated.trait_impls.set_slot(id, timpl);
            }
            TransItemSourceKind::DropGlueMethod => {
                let Some(AnyTransId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_drop_method(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::VTable => {
                let Some(AnyTransId::Type(id)) = trans_id else {
                    unreachable!()
                };
                let ty_decl = bt_ctx.translate_vtable_struct(id, item_meta, &def)?;
                self.translated.type_decls.set_slot(id, ty_decl);
            }
            TransItemSourceKind::VTableInstance(maybe_closure_kind) => {
                let Some(AnyTransId::Global(id)) = trans_id else {
                    unreachable!()
                };
                let global_decl =
                    bt_ctx.translate_vtable_instance(id, item_meta, &def, *maybe_closure_kind)?;
                self.translated.global_decls.set_slot(id, global_decl);
            }
            TransItemSourceKind::VTableInstanceBody(maybe_closure_kind) => {
                let Some(AnyTransId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_vtable_instance_body(
                    id,
                    item_meta,
                    &def,
                    *maybe_closure_kind,
                )?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
            TransItemSourceKind::VTableShim => {
                let Some(AnyTransId::Fun(id)) = trans_id else {
                    unreachable!()
                };
                let fun_decl = bt_ctx.translate_vtable_shim(id, item_meta, &def)?;
                self.translated.fun_decls.set_slot(id, fun_decl);
            }
        }
        Ok(())
    }
}

impl ItemTransCtx<'_, '_> {
    /// Register the items inside this module or inherent impl.
    // TODO: we may want to accumulate the set of modules we found, to check that all
    // the opaque modules given as arguments actually exist
    #[tracing::instrument(skip(self, item_meta))]
    pub(crate) fn register_module(&mut self, item_meta: ItemMeta, def: &hax::FullDef) {
        if !item_meta.opacity.is_transparent() {
            return;
        }
        match def.kind() {
            hax::FullDefKind::InherentImpl { items, .. } => {
                for (_, item_def) in items {
                    self.t_ctx.enqueue_item(&item_def.def_id);
                }
            }
            hax::FullDefKind::Mod { items, .. } => {
                for (_, def_id) in items {
                    self.t_ctx.enqueue_item(def_id);
                }
            }
            hax::FullDefKind::ForeignMod { items, .. } => {
                for def_id in items {
                    self.t_ctx.enqueue_item(def_id);
                }
            }
            _ => panic!("Item should be a module but isn't: {def:?}"),
        }
    }

    pub(crate) fn get_item_kind(
        &mut self,
        span: Span,
        def: &hax::FullDef,
    ) -> Result<ItemKind, Error> {
        let assoc = match def.kind() {
            hax::FullDefKind::AssocTy {
                associated_item, ..
            }
            | hax::FullDefKind::AssocConst {
                associated_item, ..
            }
            | hax::FullDefKind::AssocFn {
                associated_item, ..
            } => associated_item,
            hax::FullDefKind::Closure { args, .. } => {
                let info = self.translate_closure_info(span, args)?;
                return Ok(ItemKind::Closure { info });
            }
            _ => return Ok(ItemKind::TopLevel),
        };
        Ok(match &assoc.container {
            // E.g.:
            // ```
            // impl<T> List<T> {
            //   fn new() -> Self { ... } <- inherent method
            // }
            // ```
            hax::AssocItemContainer::InherentImplContainer { .. } => ItemKind::TopLevel,
            // E.g.:
            // ```
            // impl Foo for Bar {
            //   fn baz(...) { ... } // <- implementation of a trait method
            // }
            // ```
            hax::AssocItemContainer::TraitImplContainer {
                impl_,
                implemented_trait_ref,
                implemented_trait_item,
                overrides_default,
                ..
            } => {
                let impl_ref = self.translate_trait_impl_ref(span, impl_)?;
                let trait_ref = self.translate_trait_ref(span, implemented_trait_ref)?;
                if matches!(def.kind(), hax::FullDefKind::AssocFn { .. }) {
                    // Ensure we translate the corresponding decl signature.
                    // FIXME(self_clause): also ensure we translate associated globals
                    // consistently; to do once we have clearer `Self` clause handling.
                    let _ = self.register_fun_decl_id(span, implemented_trait_item);
                }
                let item_name = self.t_ctx.translate_trait_item_name(def.def_id())?;
                ItemKind::TraitImpl {
                    impl_ref,
                    trait_ref,
                    item_name,
                    reuses_default: !overrides_default,
                }
            }
            // This method is the *declaration* of a trait item
            // E.g.:
            // ```
            // trait Foo {
            //   fn baz(...); // <- declaration of a trait method
            // }
            // ```
            hax::AssocItemContainer::TraitContainer { trait_ref, .. } => {
                // The trait id should be Some(...): trait markers (that we may eliminate)
                // don't have associated items.
                let trait_ref = self.translate_trait_ref(span, trait_ref)?;
                let item_name = self.t_ctx.translate_trait_item_name(def.def_id())?;
                ItemKind::TraitDecl {
                    trait_ref,
                    item_name,
                    has_default: assoc.has_value,
                }
            }
        })
    }

    /// Translate a type definition.
    ///
    /// Note that we translate the types one by one: we don't need to take into
    /// account the fact that some types are mutually recursive at this point
    /// (we will need to take that into account when generating the code in a file).
    #[tracing::instrument(skip(self, item_meta))]
    pub fn translate_type_decl(
        mut self,
        trans_id: TypeDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TypeDecl, Error> {
        let span = item_meta.span;

        // Translate generics and predicates
        self.translate_def_generics(span, def)?;

        // Get the kind of the type decl -- is it a closure?
        let src = self.get_item_kind(span, def)?;

        // Translate type body
        let kind = match &def.kind {
            _ if item_meta.opacity.is_opaque() => Ok(TypeDeclKind::Opaque),
            hax::FullDefKind::OpaqueTy | hax::FullDefKind::ForeignTy => Ok(TypeDeclKind::Opaque),
            hax::FullDefKind::TyAlias { ty, .. } => {
                // Don't error on missing trait refs.
                self.error_on_impl_expr_error = false;
                self.translate_ty(span, ty).map(TypeDeclKind::Alias)
            }
            hax::FullDefKind::Adt { .. } => self.translate_adt_def(trans_id, span, &item_meta, def),
            hax::FullDefKind::Closure { args, .. } => {
                self.translate_closure_adt(trans_id, span, &args)
            }
            _ => panic!("Unexpected item when translating types: {def:?}"),
        };

        let kind = match kind {
            Ok(kind) => kind,
            Err(err) => TypeDeclKind::Error(err.msg),
        };
        let layout = self.translate_layout(def.def_id());
        let ptr_metadata = self.translate_ptr_metadata(def.def_id());
        let type_def = TypeDecl {
            def_id: trans_id,
            item_meta,
            generics: self.into_generics(),
            kind,
            src,
            layout,
            ptr_metadata,
        };

        Ok(type_def)
    }

    /// Translate one function.
    #[tracing::instrument(skip(self, item_meta))]
    pub fn translate_function(
        mut self,
        def_id: FunDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<FunDecl, Error> {
        trace!("About to translate function:\n{:?}", def.def_id);
        let span = item_meta.span;

        // Translate the function signature
        trace!("Translating function signature");
        let signature = self.translate_function_signature(def, &item_meta)?;

        // Check whether this function is a method declaration for a trait definition.
        // If this is the case, it shouldn't contain a body.
        let kind = self.get_item_kind(span, def)?;
        let is_trait_method_decl_without_default = match &kind {
            ItemKind::TraitDecl { has_default, .. } => !has_default,
            _ => false,
        };

        let is_global_initializer = matches!(
            def.kind(),
            hax::FullDefKind::Const { .. }
                | hax::FullDefKind::AssocConst { .. }
                | hax::FullDefKind::Static { .. }
        );
        let is_global_initializer =
            is_global_initializer.then(|| self.register_global_decl_id(span, &def.def_id));

        let body = if item_meta.opacity.with_private_contents().is_opaque()
            || is_trait_method_decl_without_default
        {
            Err(Opaque)
        } else if let hax::FullDefKind::Ctor {
            adt_def_id,
            ctor_of,
            variant_id,
            fields,
            output_ty,
            ..
        } = def.kind()
        {
            let body = self.build_ctor_body(
                span,
                &signature,
                adt_def_id,
                ctor_of,
                *variant_id,
                fields,
                output_ty,
            )?;
            Ok(body)
        } else {
            // Translate the body. This doesn't store anything if we can't/decide not to translate
            // this body.
            let mut bt_ctx = BodyTransCtx::new(&mut self);
            match bt_ctx.translate_def_body(item_meta.span, def) {
                Ok(Ok(body)) => Ok(body),
                // Opaque declaration
                Ok(Err(Opaque)) => Err(Opaque),
                // Translation error.
                // FIXME: handle error cases more explicitly.
                Err(_) => Err(Opaque),
            }
        };
        Ok(FunDecl {
            def_id,
            item_meta,
            signature,
            kind,
            is_global_initializer,
            body,
        })
    }

    /// Translate one global.
    #[tracing::instrument(skip(self, item_meta))]
    pub fn translate_global(
        mut self,
        def_id: GlobalDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<GlobalDecl, Error> {
        trace!("About to translate global:\n{:?}", def.def_id);
        let span = item_meta.span;

        // Translate the generics and predicates - globals *can* have generics
        // Ex.:
        // ```
        // impl<const N : usize> Foo<N> {
        //   const LEN : usize = N;
        // }
        // ```
        self.translate_def_generics(span, def)?;

        // Retrieve the kind
        let item_kind = self.get_item_kind(span, def)?;

        trace!("Translating global type");
        let ty = match &def.kind {
            hax::FullDefKind::Const { ty, .. }
            | hax::FullDefKind::AssocConst { ty, .. }
            | hax::FullDefKind::Static { ty, .. } => ty,
            _ => panic!("Unexpected def for constant: {def:?}"),
        };
        let ty = self.translate_ty(span, ty)?;

        let global_kind = match &def.kind {
            hax::FullDefKind::Static { .. } => GlobalKind::Static,
            hax::FullDefKind::Const {
                kind: hax::ConstKind::TopLevel,
                ..
            }
            | hax::FullDefKind::AssocConst { .. } => GlobalKind::NamedConst,
            hax::FullDefKind::Const { .. } => GlobalKind::AnonConst,
            _ => panic!("Unexpected def for constant: {def:?}"),
        };

        let initializer = self.register_fun_decl_id(span, &def.def_id);

        Ok(GlobalDecl {
            def_id,
            item_meta,
            generics: self.into_generics(),
            ty,
            kind: item_kind,
            global_kind,
            init: initializer,
        })
    }

    pub fn trait_id_is_dyn_compatible(&mut self, trait_id: &hax::DefId) -> bool {
        let rid = trait_id.as_rust_def_id().unwrap();
        self.t_ctx.tcx.is_dyn_compatible(rid)
    }

    /// DFS over the trait's ***inheritance hierarchy*** and get all the possible associated types.
    /// Returns the list of references to these types.
    /// I.e., returns a list of `TraitType`s
    /// 
    /// These types are used as the additional generic arguments of the vtable struct.
    /// Also serves as a template of parameters of the vtable struct declaration.
    pub fn prepare_trait_assoc_types(&mut self, trait_id: &hax::DefId) -> Result<Vec<Ty>, Error> {
        let full_def = self.hax_def(trait_id)?;
        let hax::FullDefKind::Trait {
            implied_predicates,
            items,
            self_predicate,
            ..
        } = full_def.kind()
        else {
            unreachable!("Expected a trait definition, got: {full_def:?}");
        };
        let mut assoc_types = Vec::new();

        let span = self.def_span(trait_id);
        let mut self_predicate = self.translate_trait_predicate(span, self_predicate)?;
        self_predicate.generics = self_predicate.generics.move_under_binder();
        let self_predicate = RegionBinder::empty(self_predicate);

        // Collect from the parent traits
        // simulation of the actual clause id for the parent trait clauses
        let mut clause_id: TraitClauseId = TraitClauseId::ZERO;
        for (clause, _) in implied_predicates.predicates.iter() {
            match clause.kind.hax_skip_binder_ref() {
                hax::ClauseKind::Trait(pred) => {
                    // tell if `clause` is a parent trait clause
                    if !is_hax_parent_trait_ref(&pred.trait_ref) {
                        clause_id += 1;
                        continue;
                    }
                    for ty in self.prepare_trait_assoc_types(&pred.trait_ref.def_id)? {
                        // to add the parent trait info further to this
                        let TyKind::TraitType(tref, name) = ty.kind() else {
                            unreachable!();
                        };
                        // as `ParentClause` is expressed inner-out, we need to push the
                        // self predicate to the inner-most trait ref
                        fn push_self_trait_to_internal(
                            parent_tref: &TraitRef,
                            clause_id: TraitClauseId,
                            self_predicate: &RegionBinder<TraitDeclRef>,
                        ) -> TraitRef {
                            match &parent_tref.kind {
                                TraitRefKind::ParentClause(inner_parent, id) => {
                                    TraitRef {
                                        kind: TraitRefKind::ParentClause(
                                            Box::new(push_self_trait_to_internal(inner_parent, *id, self_predicate)),
                                            *id,
                                        ),
                                        trait_decl_ref: parent_tref.trait_decl_ref.clone(),
                                    }
                                }
                                // in other cases, this is the inner-most trait ref
                                TraitRefKind::SelfId => {
                                    TraitRef {
                                        kind: TraitRefKind::ParentClause(
                                            Box::new(TraitRef {
                                                kind: TraitRefKind::SelfId,
                                                trait_decl_ref: self_predicate.clone(),
                                            }),
                                            clause_id,
                                        ),
                                        trait_decl_ref: parent_tref.trait_decl_ref.clone(),
                                    }
                                }
                                _ => {
                                    unreachable!()
                                }
                            }
                        }

                        let modified_tref = push_self_trait_to_internal(tref, clause_id, &self_predicate);
                        assoc_types.push(Ty::new(TyKind::TraitType(
                            modified_tref,
                            name.clone(),
                        )));
                    }
                    clause_id += 1;
                }
                // otherwise, it is not a parent trait clause anyway
                // no need to update the clause id here, as in `self.register_predicates`
                // the `Trait` kinds are handled firstly
                _ => {}
            }
        }

        // Collect for itself
        for (_, def) in items {
            match def.kind() {
                hax::FullDefKind::AssocTy { .. } => {
                    let name = self.t_ctx.translate_trait_item_name(def.def_id())?;
                    assoc_types.push(Ty::new(TyKind::TraitType(
                        TraitRef {
                            kind: TraitRefKind::SelfId,
                            trait_decl_ref: self_predicate.clone(),
                        },
                        name,
                    )));
                }
                _ => continue,
            }
        }

        Ok(assoc_types)
    }

    /// Given a trait id, return the vtable struct reference for this trait whose
    /// generic arguments are the parameters of the trait.
    ///
    /// If the current trait is not dyn-compatible, return `None`.
    ///
    /// NOTE: only use in `trait decl` translation
    pub fn get_vtable_struct_id_args_ref(
        &mut self,
        span: Span,
        trait_id: &hax::DefId,
    ) -> Result<Option<TypeDeclRef>, Error> {
        if !self.trait_id_is_dyn_compatible(trait_id) {
            return Ok(None);
        }
        let assoc_tys = self.prepare_trait_assoc_types(trait_id)?;
        // register the id and no enqueue it
        let vtable_id = *self
            .register_id_no_enqueue(
                span,
                TransItemSource {
                    def_id: trait_id.clone(),
                    kind: TransItemSourceKind::VTable,
                },
            )
            .unwrap()
            .as_type()
            .unwrap();
        let mut args = self.outermost_binder().params.identity_args();
        // Remove the `Self` type variable from the generic parameters
        args.types.remove_and_shift_ids(TypeVarId::ZERO);
        // The associated types are the generic arguments of the vtable struct
        args.types.extend(assoc_tys);
        Ok(Some(TypeDeclRef {
            id: TypeId::Adt(vtable_id),
            generics: Box::new(args),
        }))
    }

    #[tracing::instrument(skip(self, item_meta))]
    pub fn translate_trait_decl(
        mut self,
        def_id: TraitDeclId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TraitDecl, Error> {
        trace!("About to translate trait decl:\n{:?}", def.def_id);
        trace!("Trait decl id:\n{:?}", def_id);

        let span = item_meta.span;

        // Translate the generics
        // Note that in the generics returned by [translate_def_generics], the trait refs only
        // contain the local trait clauses. The parent clauses are stored in
        // `self.parent_trait_clauses`.
        self.translate_def_generics(span, def)?;

        if let hax::FullDefKind::TraitAlias { .. } = def.kind() {
            // Trait aliases don't have any items. Everything interesting is in the parent clauses.
            return Ok(TraitDecl {
                def_id,
                item_meta,
                parent_clauses: into_trait_clause_vec(mem::take(&mut self.parent_trait_clauses)),
                generics: self.into_generics(),
                type_clauses: Default::default(),
                consts: Default::default(),
                const_defaults: Default::default(),
                types: Default::default(),
                type_defaults: Default::default(),
                methods: Default::default(),
                // Trait aliases will be expanded when used.
                // No vtable on its own, just use those for its parents
                vtable: None,
            });
        }

        let vtable = self.get_vtable_struct_id_args_ref(span, &def.def_id)?;

        let hax::FullDefKind::Trait {
            items,
            self_predicate,
            ..
        } = &def.kind
        else {
            raise_error!(self, span, "Unexpected definition: {def:?}");
        };
        let self_trait_ref = TraitRef {
            kind: TraitRefKind::SelfId,
            trait_decl_ref: RegionBinder::empty(
                self.translate_trait_predicate(span, self_predicate)?,
            ),
        };
        let items: Vec<(TraitItemName, &hax::AssocItem, Arc<hax::FullDef>)> = items
            .iter()
            .map(|(item, def)| {
                let name = self.t_ctx.translate_trait_item_name(def.def_id())?;
                Ok((name, item, def.clone()))
            })
            .try_collect()?;

        // Translate the associated items
        // We do something subtle here: TODO: explain
        let mut consts = Vec::new();
        let mut const_defaults = IndexMap::new();
        let mut types = Vec::new();
        let mut type_clauses = Vec::new();
        let mut type_defaults = IndexMap::new();
        let mut methods = Vec::new();
        for (item_name, hax_item, hax_def) in &items {
            let item_def_id = &hax_item.def_id;
            let item_span = self.def_span(item_def_id);
            match &hax_def.kind {
                hax::FullDefKind::AssocFn { .. } => {
                    let fun_def = self.hax_def(item_def_id)?;
                    let binder_kind = BinderKind::TraitMethod(def_id, item_name.clone());
                    let mut fn_ref = self.translate_binder_for_def(
                        item_span,
                        binder_kind,
                        &fun_def,
                        |bt_ctx| {
                            // If the trait is opaque, we only translate the signature of a method
                            // with default body if it's overridden or used somewhere else.
                            // We insert the `Binder<FunDeclRef>` unconditionally here, and remove
                            // the ones that correspond to untranslated functions in the
                            // `remove_unused_methods` pass.
                            // FIXME: this triggers the translation of traits used in the method
                            // clauses, despite the fact that we may end up not needing them.
                            let fun_id = if bt_ctx.t_ctx.options.translate_all_methods
                                || item_meta.opacity.is_transparent()
                                || !hax_item.has_value
                            {
                                bt_ctx.register_fun_decl_id(item_span, item_def_id)
                            } else {
                                bt_ctx.register_fun_decl_id_no_enqueue(item_span, item_def_id)
                            };

                            assert_eq!(bt_ctx.binding_levels.len(), 2);
                            let fun_generics = bt_ctx
                                .outermost_binder()
                                .params
                                .identity_args_at_depth(DeBruijnId::one())
                                .concat(
                                    &bt_ctx
                                        .innermost_binder()
                                        .params
                                        .identity_args_at_depth(DeBruijnId::zero()),
                                );
                            Ok(FunDeclRef {
                                id: fun_id,
                                generics: Box::new(fun_generics),
                            })
                        },
                    )?;
                    // In hax, associated items take an extra explicit `Self: Trait` clause, but we
                    // don't want that to be part of the method clauses. Hence we remove the first
                    // bound clause and replace its uses with references to the ambient `Self`
                    // clause available in trait declarations.
                    {
                        struct ReplaceSelfVisitor;
                        impl VarsVisitor for ReplaceSelfVisitor {
                            fn visit_clause_var(&mut self, v: ClauseDbVar) -> Option<TraitRefKind> {
                                if let DeBruijnVar::Bound(DeBruijnId::ZERO, clause_id) = v {
                                    // Replace clause 0 and decrement the others.
                                    Some(if let Some(new_id) = clause_id.index().checked_sub(1) {
                                        TraitRefKind::Clause(DeBruijnVar::Bound(
                                            DeBruijnId::ZERO,
                                            TraitClauseId::new(new_id),
                                        ))
                                    } else {
                                        TraitRefKind::SelfId
                                    })
                                } else {
                                    None
                                }
                            }
                        }
                        fn_ref.params.visit_vars(&mut ReplaceSelfVisitor);
                        fn_ref.skip_binder.visit_vars(&mut ReplaceSelfVisitor);
                        fn_ref
                            .params
                            .trait_clauses
                            .remove_and_shift_ids(TraitClauseId::ZERO);
                        fn_ref.params.trait_clauses.iter_mut().for_each(|clause| {
                            clause.clause_id -= 1;
                        });
                    }
                    methods.push((item_name.clone(), fn_ref));
                }
                hax::FullDefKind::AssocConst { ty, .. } => {
                    // Check if the constant has a value (i.e., a body).
                    if hax_item.has_value {
                        // The parameters of the constant are the same as those of the item that
                        // declares them.
                        let id = self.register_global_decl_id(item_span, item_def_id);
                        let mut generics = self.the_only_binder().params.identity_args();
                        generics.trait_refs.push(self_trait_ref.clone());
                        let gref = GlobalDeclRef {
                            id,
                            generics: Box::new(generics),
                        };
                        const_defaults.insert(item_name.clone(), gref);
                    };
                    let ty = self.translate_ty(item_span, ty)?;
                    consts.push((item_name.clone(), ty));
                }
                hax::FullDefKind::AssocTy { param_env, .. }
                    if !param_env.generics.params.is_empty() =>
                {
                    raise_error!(
                        self,
                        item_span,
                        "Generic associated types are not supported"
                    );
                }
                hax::FullDefKind::AssocTy { value, .. } => {
                    // TODO: handle generics (i.e. GATs).
                    if let Some(clauses) = self.item_trait_clauses.get(item_name) {
                        type_clauses.push((item_name.clone(), clauses.clone()));
                    }
                    if let Some(ty) = value {
                        let ty = self.translate_ty(item_span, &ty)?;
                        type_defaults.insert(item_name.clone(), ty);
                    };
                    types.push(item_name.clone());
                }
                _ => panic!("Unexpected definition for trait item: {hax_def:?}"),
            }
        }

        // In case of a trait implementation, some values may not have been
        // provided, in case the declaration provided default values. We
        // check those, and lookup the relevant values.
        Ok(TraitDecl {
            def_id,
            item_meta,
            parent_clauses: into_trait_clause_vec(mem::take(&mut self.parent_trait_clauses)),
            generics: self.into_generics(),
            type_clauses,
            consts,
            const_defaults,
            types,
            type_defaults,
            methods,
            vtable,
        })
    }

    pub fn get_vtable_instance_ref(
        &mut self,
        span: Span,
        trait_def_id: &hax::DefId,
        implemented_trait_args: &Box<GenericArgs>,
        impl_def_id: &hax::DefId,
        maybe_closure_kind: Option<ClosureKind>,
    ) -> Option<GlobalDeclRef> {
        if self.trait_id_is_dyn_compatible(trait_def_id) {
            // Register the vtable instance for this impl.
            let id = self.register_vtable_instance_as_global_decl_id(
                span,
                impl_def_id,
                maybe_closure_kind,
            );
            let mut generics = implemented_trait_args.clone();
            // Remove the `Self` type variable from the generic parameters.
            generics.types.remove_and_shift_ids(TypeVarId::ZERO);
            Some(GlobalDeclRef { id, generics })
        } else {
            None
        }
    }

    #[tracing::instrument(skip(self, item_meta))]
    pub fn translate_trait_impl(
        mut self,
        def_id: TraitImplId,
        item_meta: ItemMeta,
        def: &hax::FullDef,
    ) -> Result<TraitImpl, Error> {
        trace!("About to translate trait impl:\n{:?}", def.def_id);
        trace!("Trait impl id:\n{:?}", def_id);

        let span = item_meta.span;

        self.translate_def_generics(span, def)?;

        if let hax::FullDefKind::TraitAlias { .. } = def.kind() {
            // Generate a blanket impl for this trait, as in:
            //   trait Alias<U> = Trait<Option<U>, Item = u32> + Clone;
            // becomes:
            //   trait Alias<U>: Trait<Option<U>, Item = u32> + Clone {}
            //   impl<U, Self: Trait<Option<U>, Item = u32> + Clone> Alias<U> for Self {}

            // `translate_def_generics` registers the clauses as implied clauses, but we want them
            // as required clauses for the impl.
            assert!(self.innermost_generics_mut().trait_clauses.is_empty());
            let clauses = into_trait_clause_vec(mem::take(&mut self.parent_trait_clauses));
            self.innermost_generics_mut().trait_clauses = clauses;
            let trait_id = self.register_trait_decl_id(span, def.def_id());
            let mut generics = self.the_only_binder().params.identity_args();
            // Do the inverse operation: the trait considers the clauses as implied.
            let parent_trait_refs = mem::take(&mut generics.trait_refs);
            let implemented_trait = TraitDeclRef {
                id: trait_id,
                generics: Box::new(generics),
            };
            let mut timpl = TraitImpl {
                def_id,
                item_meta,
                impl_trait: implemented_trait,
                generics: self.the_only_binder().params.clone(),
                parent_trait_refs,
                type_clauses: Default::default(),
                consts: Default::default(),
                types: Default::default(),
                methods: Default::default(),
                vtable_instance: None,
            };
            // We got the predicates from a trait decl, so they may refer to the virtual `Self`
            // clause, which doesn't exist for impls. We fix that up here.
            {
                struct FixSelfVisitor {
                    binder_depth: DeBruijnId,
                }
                struct UnhandledSelf;
                impl Visitor for FixSelfVisitor {
                    type Break = UnhandledSelf;
                }
                impl VisitAstMut for FixSelfVisitor {
                    fn enter_region_binder<T: AstVisitable>(&mut self, _: &mut RegionBinder<T>) {
                        self.binder_depth = self.binder_depth.incr()
                    }
                    fn exit_region_binder<T: AstVisitable>(&mut self, _: &mut RegionBinder<T>) {
                        self.binder_depth = self.binder_depth.decr()
                    }
                    fn enter_binder<T: AstVisitable>(&mut self, _: &mut Binder<T>) {
                        self.binder_depth = self.binder_depth.incr()
                    }
                    fn exit_binder<T: AstVisitable>(&mut self, _: &mut Binder<T>) {
                        self.binder_depth = self.binder_depth.decr()
                    }
                    fn visit_trait_ref_kind(
                        &mut self,
                        kind: &mut TraitRefKind,
                    ) -> ControlFlow<Self::Break> {
                        match kind {
                            TraitRefKind::SelfId => return ControlFlow::Break(UnhandledSelf),
                            TraitRefKind::ParentClause(sub, clause_id)
                                if matches!(sub.kind, TraitRefKind::SelfId) =>
                            {
                                *kind = TraitRefKind::Clause(DeBruijnVar::bound(
                                    self.binder_depth,
                                    *clause_id,
                                ))
                            }
                            _ => (),
                        }
                        self.visit_inner(kind)
                    }
                }
                match timpl.drive_mut(&mut FixSelfVisitor {
                    binder_depth: DeBruijnId::zero(),
                }) {
                    ControlFlow::Continue(()) => {}
                    ControlFlow::Break(UnhandledSelf) => {
                        register_error!(
                            self,
                            span,
                            "Found `Self` clause we can't handle \
                             in a trait alias blanket impl."
                        );
                    }
                }
            };
            return Ok(timpl);
        }

        let hax::FullDefKind::TraitImpl {
            trait_pred,
            implied_impl_exprs,
            items: impl_items,
            ..
        } = &def.kind
        else {
            unreachable!()
        };

        // Retrieve the information about the implemented trait.
        let implemented_trait = self.translate_trait_ref(span, &trait_pred.trait_ref)?;
        let trait_id = implemented_trait.id;
        // A `TraitRef` that points to this impl with the correct generics.
        let self_predicate = TraitRef {
            kind: TraitRefKind::TraitImpl(TraitImplRef {
                id: def_id,
                generics: Box::new(self.the_only_binder().params.identity_args()),
            }),
            trait_decl_ref: RegionBinder::empty(implemented_trait.clone()),
        };

        let vtable_instance = self.get_vtable_instance_ref(
            span,
            &trait_pred.trait_ref.def_id,
            &implemented_trait.generics,
            def.def_id(),
            None,
        );
        trace!(
            "Get vtable instance done for usual impl: {}",
            match &vtable_instance {
                Some(vtable) => vtable.with_ctx(&self.into_fmt()).to_string(),
                None => "None".to_owned(),
            }
        );

        // The trait refs which implement the parent clauses of the implemented trait decl.
        let parent_trait_refs = self.translate_trait_impl_exprs(span, &implied_impl_exprs)?;

        {
            // Debugging
            let ctx = self.into_fmt();
            let refs = parent_trait_refs
                .iter()
                .map(|c| c.with_ctx(&ctx))
                .format("\n");
            trace!(
                "Trait impl: {:?}\n- parent_trait_refs:\n{}",
                def.def_id, refs
            );
        }

        // Explore the associated items
        let mut consts = Vec::new();
        let mut types: Vec<(TraitItemName, Ty)> = Vec::new();
        let mut methods = Vec::new();
        let mut type_clauses = Vec::new();

        for impl_item in impl_items {
            use hax::ImplAssocItemValue::*;
            let name = self
                .t_ctx
                .translate_trait_item_name(impl_item.decl_def.def_id())?;
            let item_def = impl_item.def(); // The impl item or the corresponding trait default.
            let item_span = self.def_span(&item_def.def_id);
            let item_def_id = &item_def.def_id;
            match item_def.kind() {
                hax::FullDefKind::AssocFn { .. } => {
                    match &impl_item.value {
                        Provided { is_override, .. } => {
                            let fun_def = self.hax_def(item_def_id)?;
                            let binder_kind = BinderKind::TraitMethod(trait_id, name.clone());
                            let fn_ref = self.translate_binder_for_def(
                                item_span,
                                binder_kind,
                                &fun_def,
                                |bt_ctx| {
                                    // If the impl is opaque, we only translate the signature of a
                                    // method with a default body if it's directly used somewhere
                                    // else.
                                    // We insert the `Binder<FunDeclRef>` unconditionally here, and
                                    // remove the ones that correspond to untranslated functions in
                                    // the `remove_unused_methods` pass.
                                    let fun_id = if bt_ctx.t_ctx.options.translate_all_methods
                                        || item_meta.opacity.is_transparent()
                                        || !*is_override
                                    {
                                        bt_ctx.register_fun_decl_id(item_span, item_def_id)
                                    } else {
                                        bt_ctx
                                            .register_fun_decl_id_no_enqueue(item_span, item_def_id)
                                    };

                                    // TODO: there's probably a cleaner way to write this
                                    assert_eq!(bt_ctx.binding_levels.len(), 2);
                                    let fun_generics = bt_ctx
                                        .outermost_binder()
                                        .params
                                        .identity_args_at_depth(DeBruijnId::one())
                                        .concat(
                                            &bt_ctx
                                                .innermost_binder()
                                                .params
                                                .identity_args_at_depth(DeBruijnId::zero()),
                                        );
                                    Ok(FunDeclRef {
                                        id: fun_id,
                                        generics: Box::new(fun_generics),
                                    })
                                },
                            )?;
                            methods.push((name, fn_ref));
                        }
                        DefaultedFn { .. } => {
                            // TODO: handle defaulted methods
                        }
                        _ => unreachable!(),
                    }
                }
                hax::FullDefKind::AssocConst { .. } => {
                    let id = self.register_global_decl_id(item_span, item_def_id);
                    // The parameters of the constant are the same as those of the item that
                    // declares them.
                    let generics = match &impl_item.value {
                        Provided { .. } => self.the_only_binder().params.identity_args(),
                        _ => {
                            let mut generics = implemented_trait.generics.as_ref().clone();
                            generics.trait_refs.push(self_predicate.clone());
                            generics
                        }
                    };
                    let gref = GlobalDeclRef {
                        id,
                        generics: Box::new(generics),
                    };
                    consts.push((name, gref));
                }
                hax::FullDefKind::AssocTy { param_env, .. }
                    if !param_env.generics.params.is_empty() =>
                {
                    // We don't support GATs; the error was already reported in the trait declaration.
                }
                hax::FullDefKind::AssocTy { value, .. } => {
                    let ty = match &impl_item.value {
                        Provided { .. } => value.as_ref().unwrap(),
                        DefaultedTy { ty, .. } => ty,
                        _ => unreachable!(),
                    };
                    let ty = self.translate_ty(item_span, &ty)?;
                    types.push((name.clone(), ty));

                    let trait_refs =
                        self.translate_trait_impl_exprs(item_span, &impl_item.required_impl_exprs)?;
                    type_clauses.push((name, trait_refs));
                }
                _ => panic!("Unexpected definition for trait item: {item_def:?}"),
            }
        }

        Ok(TraitImpl {
            def_id,
            item_meta,
            impl_trait: implemented_trait,
            generics: self.into_generics(),
            parent_trait_refs,
            type_clauses,
            consts,
            types,
            methods,
            vtable_instance,
        })
    }
}
