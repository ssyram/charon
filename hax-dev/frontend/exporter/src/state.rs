use crate::prelude::*;
use paste::paste;

macro_rules! mk_aux {
    ($state:ident {$($lts:lifetime)*} $field:ident {$($field_type:tt)+} {$($gen:tt)*} {$($gen_full:tt)*} {$($params:tt)*} {$($fields:tt)*}) => {
        paste ! {
            pub trait [<Has $field:camel>]<$($lts,)*> {
                fn $field(self: &Self) -> $($field_type)+<$($lts)*>;
            }
            impl<$($lts,)*$($gen)*> [<Has $field:camel>]<$($lts,)*> for $state<$($params)*> {
                fn $field(self: &Self) -> $($field_type)+<$($lts)*> {
                    self.$field.clone()
                }
            }
        }
    };
}
macro_rules! mk {
    (struct $state:ident<$($glts:lifetime),*> {$($field:ident : {$($lts:lifetime),*} $field_type:ty),*$(,)?}) => {
        mk!(@$state {} {$($field)*} {$($field: {$($lts),*} {$field_type},)*});
    };
    (@$state:ident {$($acc:tt)*} $fields:tt {
        $field:ident : $lts:tt $field_type:tt
        $(,$($rest:tt)*)?
    }) => {mk!(@$state {
        $($acc)* $fields $field: $lts $field_type,
    } $fields {$($($rest)*)?} );};
    (@$state:ident $body:tt $fields:tt {$(,)?}) => { mk! (@@ $state $body ); };
    (@@$state:ident {$({$($fields:tt)*} $field:ident : {$($lts:lifetime)*} {$($field_type:tt)+},)*}) => {
        paste! {
            #[derive(Clone)]
            pub struct $state<$([<$field:camel>],)*>{
                $(pub $field: [<$field:camel>],)*
            }
        }
        $(
            macro_rules! __inner_helper {
                ($gen:tt {$$($full_gen:tt)*} {$$($params:tt)*} $field $$($rest:tt)*) => {
                    paste! {__inner_helper!(
                        $gen {$$($full_gen)*[<$field:camel>],}
                        {$$($params)*$($field_type)+<$($lts,)*>,} $$($rest)*
                    );}
                };
                ({$$($gen:tt)*} {$$($full_gen:tt)*} {$$($params:tt)*} $i:ident $$($rest:tt)*) => {
                    paste! {__inner_helper!(
                        {$$($gen)*[<$i:camel>],} {$$($full_gen)*[<$i:camel>],}
                        {$$($params)*[<$i:camel>],} $$($rest)*
                    );}
                };
                ($gen:tt $full_gen:tt $params:tt $$(,)?) => {
                    mk_aux!($state {$($lts)*} $field {$($field_type)+} $gen $full_gen $params {$($fields)*});
                };
            }
            __inner_helper!({} {} {} $($fields)*);
        )*
    };
}

mod types {
    use crate::prelude::*;
    use rustc_middle::ty;
    use std::{cell::RefCell, sync::Arc};

    pub struct LocalContextS {
        pub vars: HashMap<rustc_middle::thir::LocalVarId, String>,
    }

    impl Default for LocalContextS {
        fn default() -> Self {
            Self::new()
        }
    }

    impl LocalContextS {
        pub fn new() -> LocalContextS {
            LocalContextS {
                vars: HashMap::new(),
            }
        }
    }

    /// Global caches
    #[derive(Default)]
    pub struct GlobalCache<'tcx> {
        /// Cache the `Span` translations.
        pub spans: HashMap<rustc_span::Span, Span>,
        /// Per-item cache.
        pub per_item: HashMap<RDefId, ItemCache<'tcx>>,
        /// A ID table session, providing fresh IDs.
        pub id_table_session: id_table::Session,
        /// Map that recovers rustc args for a given `ItemRef`.
        pub reverse_item_refs_map: HashMap<id_table::Id, ty::GenericArgsRef<'tcx>>,
    }

    /// Defines a mapping from types to types, for use with `TypeMap`.
    pub struct FullDefMapper;
    impl TypeMapper for FullDefMapper {
        type Value<Body: TypeMappable> = Arc<FullDef<Body>>;
    }

    /// Per-item cache
    #[derive(Default)]
    pub struct ItemCache<'tcx> {
        /// The translated `DefId`.
        pub def_id: Option<DefId>,
        /// The translated definitions, generic in the Body kind.
        /// Each rustc `DefId` gives several hax `DefId`s: one for each promoted constant (if any),
        /// and the base one represented by `None`. Moreover we can instantiate definitions with
        /// generic arguments.
        pub full_defs:
            HashMap<(Option<PromotedId>, Option<ty::GenericArgsRef<'tcx>>), TypeMap<FullDefMapper>>,
        /// Cache the `Ty` translations.
        pub tys: HashMap<ty::Ty<'tcx>, Ty>,
        /// Cache the `ItemRef` translations. This is fast because `GenericArgsRef` is interned.
        pub item_refs: HashMap<(RDefId, ty::GenericArgsRef<'tcx>), ItemRef>,
        /// Cache the trait resolution engine for each item.
        pub predicate_searcher: Option<crate::traits::PredicateSearcher<'tcx>>,
        /// Cache of trait refs to resolved impl expressions.
        pub impl_exprs: HashMap<ty::PolyTraitRef<'tcx>, crate::traits::ImplExpr>,
    }

    #[derive(Clone)]
    pub struct Base<'tcx> {
        pub options: Rc<hax_frontend_exporter_options::Options>,
        pub local_ctx: Rc<RefCell<LocalContextS>>,
        pub opt_def_id: Option<rustc_hir::def_id::DefId>,
        pub cache: Rc<RefCell<GlobalCache<'tcx>>>,
        pub tcx: ty::TyCtxt<'tcx>,
        /// Rust doesn't enforce bounds on generic parameters in type
        /// aliases. Thus, when translating type aliases, we need to
        /// disable the resolution of implementation expressions. For
        /// more details, please see
        /// https://github.com/hacspec/hax/issues/707.
        pub ty_alias_mode: bool,
    }

    impl<'tcx> Base<'tcx> {
        pub fn new(
            tcx: rustc_middle::ty::TyCtxt<'tcx>,
            options: hax_frontend_exporter_options::Options,
        ) -> Self {
            Self {
                tcx,
                cache: Default::default(),
                options: Rc::new(options),
                // Always prefer `s.owner_id()` to `s.base().opt_def_id`.
                // `opt_def_id` is used in `utils` for error reporting
                opt_def_id: None,
                local_ctx: Rc::new(RefCell::new(LocalContextS::new())),
                ty_alias_mode: false,
            }
        }
    }

    pub type MacroCalls = Rc<HashMap<Span, Span>>;
    pub type RcThir<'tcx> = Rc<rustc_middle::thir::Thir<'tcx>>;
    pub type RcMir<'tcx> = Rc<rustc_middle::mir::Body<'tcx>>;
    pub type UnitBinder<'tcx> = rustc_middle::ty::Binder<'tcx, ()>;
}

mk!(
    struct State<'tcx> {
        base: {'tcx} types::Base,
        owner_id: {} rustc_hir::def_id::DefId,
        thir: {'tcx} types::RcThir,
        mir: {'tcx} types::RcMir,
        binder: {'tcx} types::UnitBinder,
        ty: {'tcx} rustc_middle::ty::Ty,
    }
);

pub use self::types::*;

pub type StateWithBase<'tcx> = State<Base<'tcx>, (), (), (), (), ()>;
pub type StateWithOwner<'tcx> = State<Base<'tcx>, rustc_hir::def_id::DefId, (), (), (), ()>;
pub type StateWithBinder<'tcx> =
    State<Base<'tcx>, rustc_hir::def_id::DefId, (), (), types::UnitBinder<'tcx>, ()>;
pub type StateWithThir<'tcx> =
    State<Base<'tcx>, rustc_hir::def_id::DefId, types::RcThir<'tcx>, (), (), ()>;
pub type StateWithThirAndTy<'tcx> = State<
    Base<'tcx>,
    rustc_hir::def_id::DefId,
    types::RcThir<'tcx>,
    (),
    (),
    rustc_middle::ty::Ty<'tcx>,
>;
pub type StateWithMir<'tcx> =
    State<Base<'tcx>, rustc_hir::def_id::DefId, (), types::RcMir<'tcx>, (), ()>;

impl<'tcx> StateWithBase<'tcx> {
    pub fn new(
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        options: hax_frontend_exporter_options::Options,
    ) -> Self {
        Self {
            base: Base::new(tcx, options),
            owner_id: (),
            thir: (),
            mir: (),
            binder: (),
            ty: (),
        }
    }
}

pub trait BaseState<'tcx>: HasBase<'tcx> + Clone {
    /// Updates the OnwerId in a state, making sure to override `opt_def_id` in base as well.
    fn with_owner_id(&self, owner_id: rustc_hir::def_id::DefId) -> StateWithOwner<'tcx> {
        let mut base = self.base();
        base.opt_def_id = Some(owner_id);
        State {
            owner_id,
            base,
            thir: (),
            mir: (),
            binder: (),
            ty: (),
        }
    }
}
impl<'tcx, T: HasBase<'tcx> + Clone> BaseState<'tcx> for T {}

/// State of anything below an `owner`.
pub trait UnderOwnerState<'tcx>: BaseState<'tcx> + HasOwnerId {
    fn with_base(&self, base: types::Base<'tcx>) -> StateWithOwner<'tcx> {
        State {
            owner_id: self.owner_id(),
            base,
            thir: (),
            mir: (),
            binder: (),
            ty: (),
        }
    }
    fn with_binder(&self, binder: types::UnitBinder<'tcx>) -> StateWithBinder<'tcx> {
        State {
            base: self.base(),
            owner_id: self.owner_id(),
            binder,
            thir: (),
            mir: (),
            ty: (),
        }
    }
    fn with_thir(&self, thir: types::RcThir<'tcx>) -> StateWithThir<'tcx> {
        State {
            base: self.base(),
            owner_id: self.owner_id(),
            thir,
            mir: (),
            binder: (),
            ty: (),
        }
    }
    fn with_mir(&self, mir: types::RcMir<'tcx>) -> StateWithMir<'tcx> {
        State {
            base: self.base(),
            owner_id: self.owner_id(),
            mir,
            thir: (),
            binder: (),
            ty: (),
        }
    }
}
impl<'tcx, T: BaseState<'tcx> + HasOwnerId> UnderOwnerState<'tcx> for T {}

/// State of anything below a binder.
pub trait UnderBinderState<'tcx> = UnderOwnerState<'tcx> + HasBinder<'tcx>;

/// While translating expressions, we expect to always have a THIR
/// body and an `owner_id` in the state
pub trait ExprState<'tcx>: UnderOwnerState<'tcx> + HasThir<'tcx> {
    fn with_ty(&self, ty: rustc_middle::ty::Ty<'tcx>) -> StateWithThirAndTy<'tcx> {
        State {
            base: self.base(),
            owner_id: self.owner_id(),
            thir: self.thir(),
            mir: (),
            binder: (),
            ty,
        }
    }
}
impl<'tcx, T> ExprState<'tcx> for T where T: UnderOwnerState<'tcx> + HasThir<'tcx> {}

pub trait WithGlobalCacheExt<'tcx>: BaseState<'tcx> {
    /// Access the global cache. You must not call `sinto` within this function as this will likely
    /// result in `BorrowMut` panics.
    fn with_global_cache<T>(&self, f: impl FnOnce(&mut GlobalCache<'tcx>) -> T) -> T {
        let base = self.base();
        let mut cache = base.cache.borrow_mut();
        f(&mut *cache)
    }
    /// Access the cache for a given item. You must not call `sinto` within this function as this
    /// will likely result in `BorrowMut` panics.
    fn with_item_cache<T>(&self, def_id: RDefId, f: impl FnOnce(&mut ItemCache<'tcx>) -> T) -> T {
        self.with_global_cache(|cache| f(cache.per_item.entry(def_id).or_default()))
    }
}
impl<'tcx, S: BaseState<'tcx>> WithGlobalCacheExt<'tcx> for S {}

pub trait WithItemCacheExt<'tcx>: UnderOwnerState<'tcx> {
    /// Access the cache for the current item. You must not call `sinto` within this function as
    /// this will likely result in `BorrowMut` panics.
    fn with_cache<T>(&self, f: impl FnOnce(&mut ItemCache<'tcx>) -> T) -> T {
        self.with_item_cache(self.owner_id(), f)
    }
    fn with_predicate_searcher<T>(&self, f: impl FnOnce(&mut PredicateSearcher<'tcx>) -> T) -> T {
        self.with_cache(|cache| {
            f(cache.predicate_searcher.get_or_insert_with(|| {
                PredicateSearcher::new_for_owner(
                    self.base().tcx,
                    self.owner_id(),
                    self.base().options.bounds_options,
                )
            }))
        })
    }
}
impl<'tcx, S: UnderOwnerState<'tcx>> WithItemCacheExt<'tcx> for S {}

impl ImplInfos {
    fn from<'tcx, S: BaseState<'tcx>>(s: &S, did: rustc_hir::def_id::DefId) -> Self {
        let tcx = s.base().tcx;
        let s = &s.with_owner_id(did);

        Self {
            generics: tcx.generics_of(did).sinto(s),
            typ: tcx.type_of(did).instantiate_identity().sinto(s),
            trait_ref: tcx
                .impl_trait_ref(did)
                .map(|trait_ref| trait_ref.instantiate_identity())
                .sinto(s),
            clauses: predicates_defined_on(tcx, did).as_ref().sinto(s),
        }
    }
}

/// Returns a map from every implementation (`Impl`) `DefId`s to the
/// type they implement, plus the bounds.
pub fn impl_def_ids_to_impled_types_and_bounds<'tcx, S: BaseState<'tcx>>(
    s: &S,
) -> HashMap<DefId, ImplInfos> {
    let tcx = s.base().tcx;

    let def_ids: Vec<_> = s.with_global_cache(|cache| cache.per_item.keys().copied().collect());
    let with_parents = |mut did: rustc_hir::def_id::DefId| {
        let mut acc = vec![did];
        while let Some(parent) = tcx.opt_parent(did) {
            did = parent;
            acc.push(did);
        }
        acc.into_iter()
    };
    use itertools::Itertools;
    def_ids
        .into_iter()
        .flat_map(with_parents)
        .unique()
        .filter(|&did| {
            // keep only DefIds that corresponds to implementations
            matches!(
                tcx.def_path(did).data.last(),
                Some(rustc_hir::definitions::DisambiguatedDefPathData {
                    data: rustc_hir::definitions::DefPathData::Impl,
                    ..
                })
            )
        })
        .map(|did| (did.sinto(s), ImplInfos::from(s, did)))
        .collect()
}
