//! The translation contexts.
use super::translate_crate::RustcItem;
pub use super::translate_crate::{TraitImplSource, TransItemSource, TransItemSourceKind};
use super::translate_generics::BindingLevel;
use charon_lib::ast::*;
use charon_lib::formatter::{FmtCtx, IntoFormatter};
use charon_lib::ids::Vector;
use charon_lib::options::TranslateOptions;
use hax_frontend_exporter::{self as hax, SInto};
use rustc_middle::ty::TyCtxt;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::Arc;
use std::{fmt, mem};

// Re-export to avoid having to fix imports.
pub(crate) use charon_lib::errors::{
    DepSource, ErrorCtx, Level, error_assert, raise_error, register_error,
};

/// Translation context used while translating the crate data into our representation.
pub struct TranslateCtx<'tcx> {
    /// The Rust compiler type context
    pub tcx: TyCtxt<'tcx>,
    /// Path to the toolchain root.
    pub sysroot: PathBuf,
    /// The Hax context
    pub hax_state: hax::StateWithBase<'tcx>,

    /// The options that control translation.
    pub options: TranslateOptions,
    /// The translated data.
    pub translated: TranslatedCrate,

    /// The map from rustc id to translated id.
    pub id_map: HashMap<TransItemSource, AnyTransId>,
    /// The reverse map of ids.
    pub reverse_id_map: HashMap<AnyTransId, TransItemSource>,
    /// The reverse filename map.
    pub file_to_id: HashMap<FileName, FileId>,

    /// Context for tracking and reporting errors.
    pub errors: RefCell<ErrorCtx>,
    /// The declarations we came accross and which we haven't translated yet. We keep them sorted
    /// to make the output order a bit more stable.
    pub items_to_translate: BTreeSet<TransItemSource>,
    /// The declaration we've already processed (successfully or not).
    pub processed: HashSet<TransItemSource>,
    /// Stack of the translations currently happening. Used to avoid accidental cycles.
    pub translate_stack: Vec<AnyTransId>,
    /// Cache the names to compute them only once each.
    pub cached_names: HashMap<RustcItem, Name>,
    /// Cache the `ItemMeta`s to compute them only once each.
    pub cached_item_metas: HashMap<TransItemSource, ItemMeta>,
}

/// A translation context for items.
/// Augments the [TranslateCtx] with type-level variables.
pub(crate) struct ItemTransCtx<'tcx, 'ctx> {
    /// The definition we are currently extracting.
    pub item_src: TransItemSource,
    /// The id of the definition we are currently extracting, if there is one.
    pub item_id: Option<AnyTransId>,
    /// The translation context containing the top-level definitions/ids.
    pub t_ctx: &'ctx mut TranslateCtx<'tcx>,
    /// Whether to consider a `ImplExprAtom::Error` as an error for us. True except inside type
    /// aliases, because rust does not enforce correct trait bounds on type aliases.
    pub error_on_impl_expr_error: bool,

    /// The stack of generic parameter binders for the current context. Each binder introduces an
    /// entry in this stack, with the entry as index `0` being the innermost binder. These
    /// parameters are referenced using [`DeBruijnVar`]; see there for details.
    pub binding_levels: BindingStack<BindingLevel>,
    /// (For traits only) accumulated implied trait clauses.
    pub parent_trait_clauses: Vector<TraitClauseId, TraitClause>,
    /// (For traits only) accumulated trait clauses on associated types.
    pub item_trait_clauses: HashMap<TraitItemName, Vector<TraitClauseId, TraitClause>>,
}

/// Translates `T` into `U` using `hax`'s `SInto` trait, catching any hax panics.
pub fn catch_sinto<S, T, U>(
    s: &S,
    err: &mut ErrorCtx,
    krate: &TranslatedCrate,
    span: Span,
    x: &T,
) -> Result<U, Error>
where
    T: Debug + SInto<S, U>,
{
    let unwind_safe_s = std::panic::AssertUnwindSafe(s);
    let unwind_safe_x = std::panic::AssertUnwindSafe(x);
    std::panic::catch_unwind(move || unwind_safe_x.sinto(*unwind_safe_s)).or_else(|_| {
        raise_error!(
            err,
            crate(krate),
            span,
            "Hax panicked when translating `{x:?}`."
        )
    })
}

impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    /// Span an error and register the error.
    pub fn span_err(&self, span: Span, msg: &str, level: Level) -> Error {
        self.errors
            .borrow_mut()
            .span_err(&self.translated, span, msg, level)
    }

    /// Translates `T` into `U` using `hax`'s `SInto` trait, catching any hax panics.
    pub fn catch_sinto<S, T, U>(&mut self, s: &S, span: Span, x: &T) -> Result<U, Error>
    where
        T: Debug + SInto<S, U>,
    {
        catch_sinto(s, &mut *self.errors.borrow_mut(), &self.translated, span, x)
    }

    /// Return the polymorphic definition for this item. Use with care, prefer `hax_def` whenever
    /// possible.
    ///
    /// Used for computing names, for associated items, and for various checks.
    pub fn poly_hax_def(&mut self, def_id: &hax::DefId) -> Result<Arc<hax::FullDef>, Error> {
        self.hax_def_for_item(&RustcItem::Poly(def_id.clone()))
    }

    /// Special handling for dyn trait items in monomorphization mode.
    /// 
    /// This function provides a specialized translation path for items that contain
    /// synthetic dyn trait parameters. It attempts translation with graceful fallback
    /// to prevent crashes when hax encounters complex dyn trait scenarios.
    fn translate_dyn_trait_item(&mut self, item: &RustcItem) -> Result<Arc<hax::FullDef>, Error> {
        let RustcItem::Mono(item_ref) = item else {
            unreachable!("Expected monomorphic item for dyn trait")
        };
        
        trace!("Processing dyn trait item: {:?}", item_ref.def_id);
        
        // Attempt translation with panic protection since dyn traits can cause
        // hax to encounter edge cases that might result in panics
        let unwind_safe_s = std::panic::AssertUnwindSafe(&self.hax_state);
        let item_ref_copy = item_ref.clone();
        let result = std::panic::catch_unwind(move || item_ref_copy.instantiated_full_def(*unwind_safe_s));
        
        match result {
            Ok(def) => Ok(def),
            Err(_) => {
                // If translation fails, use fallback approach
                let span = self.def_span(item.def_id());
                register_error!(
                    self,
                    span,
                    "Hax panicked when translating dyn trait item `{:?}` - using polymorphic fallback", 
                    item.def_id()
                );
                
                // Fallback: Get the polymorphic definition instead
                let def_id = item.def_id();
                let unwind_safe_s = std::panic::AssertUnwindSafe(&self.hax_state);
                Ok(def_id.full_def(*unwind_safe_s))
            }
        }
    }
    
    /// Check if an ItemRef represents a dyn trait item by looking for synthetic parameters.
    /// 
    /// This is part of the dyn trait monomorphization workflow where we need to identify
    /// items that come from dyn trait contexts and may contain synthetic `_dyn` parameters
    /// created by hax.
    fn is_dyn_trait_item_ref(&self, item_ref: &hax::ItemRef) -> bool {
        // Look for synthetic `_dyn` parameters in the generic arguments
        let has_synthetic_params = item_ref.generic_args.iter().any(|arg| {
            matches!(
                arg,
                hax::GenericArg::Type(ty) if matches!(
                    ty.kind(),
                    hax::TyKind::Param(param_ty) if param_ty.name.as_str() == "_dyn"
                )
            )
        });
        
        // Also check for problematic bound lifetimes, specifically for Display/Formatter
        let has_problematic_lifetimes = item_ref.generic_args.iter().any(|arg| {
            matches!(
                arg,
                hax::GenericArg::Lifetime(region) if matches!(
                    &region.kind,
                    hax::RegionKind::ReBound(_, _)
                ) && format!("{:?}", item_ref.def_id).contains("Formatter")
            )
        });
        
        // Also check if this is a vtable by looking at the DefId 
        let is_vtable = format!("{:?}", item_ref.def_id).contains("vtable");
        
        let result = has_synthetic_params || has_problematic_lifetimes || is_vtable;
        if result {
            let reason = if has_synthetic_params { "synthetic params" } 
                        else if has_problematic_lifetimes { "bound lifetimes" }
                        else { "vtable" };
            trace!("Found dyn trait/vtable ItemRef: {:?} (reason: {})", item_ref, reason);
        }
        result
    }

    /// Return the definition for this item. This uses the polymorphic or monomorphic definition
    /// depending on user choice.
    pub fn hax_def_for_item(&mut self, item: &RustcItem) -> Result<Arc<hax::FullDef>, Error> {
        let def_id = item.def_id();
        let span = self.def_span(def_id);
        let is_dyn_trait = if let RustcItem::Mono(item_ref) = item {
            self.is_dyn_trait_item_ref(item_ref)
        } else {
            false
        };
        
        if let RustcItem::Mono(item_ref) = item
            && item_ref.has_param
            && !is_dyn_trait
        {
            raise_error!(self, span, "Item is not monomorphic: {item:?}")
        }
        
        // For dyn trait items, use a special translation path
        if is_dyn_trait {
            return self.translate_dyn_trait_item(item);
        }
        // Hax takes care of caching the translation.
        let unwind_safe_s = std::panic::AssertUnwindSafe(&self.hax_state);
        std::panic::catch_unwind(move || match item {
            RustcItem::Poly(def_id) => def_id.full_def(*unwind_safe_s),
            RustcItem::Mono(item_ref) => item_ref.instantiated_full_def(*unwind_safe_s),
        })
        .or_else(|_| raise_error!(self, span, "Hax panicked when translating `{def_id:?}`."))
    }

    pub(crate) fn with_def_id<F, T>(
        &mut self,
        def_id: &hax::DefId,
        item_id: Option<AnyTransId>,
        f: F,
    ) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let mut errors = self.errors.borrow_mut();
        let current_def_id = mem::replace(&mut errors.def_id, item_id);
        let current_def_id_is_local = mem::replace(&mut errors.def_id_is_local, def_id.is_local);
        drop(errors); // important: release the refcell "lock"
        let ret = f(self);
        let mut errors = self.errors.borrow_mut();
        errors.def_id = current_def_id;
        errors.def_id_is_local = current_def_id_is_local;
        ret
    }
}

impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    /// Create a new `ExecContext`.
    pub(crate) fn new(
        item_src: TransItemSource,
        item_id: Option<AnyTransId>,
        t_ctx: &'ctx mut TranslateCtx<'tcx>,
    ) -> Self {
        ItemTransCtx {
            item_src,
            item_id,
            t_ctx,
            error_on_impl_expr_error: true,
            binding_levels: Default::default(),
            parent_trait_clauses: Default::default(),
            item_trait_clauses: Default::default(),
        }
    }

    /// Whether to monomorphize items we encounter.
    pub fn monomorphize(&self) -> bool {
        matches!(self.item_src.item, RustcItem::Mono(..))
    }

    pub fn span_err(&self, span: Span, msg: &str, level: Level) -> Error {
        self.t_ctx.span_err(span, msg, level)
    }

    pub fn hax_state(&self) -> &hax::StateWithBase<'tcx> {
        &self.t_ctx.hax_state
    }

    pub fn hax_state_with_id(&self) -> hax::StateWithOwner<'tcx> {
        use hax::BaseState;
        let def_id = self.item_src.def_id().underlying_rust_def_id();
        self.t_ctx.hax_state.clone().with_owner_id(def_id)
    }

    /// Return the definition for this item. This uses the polymorphic or monomorphic definition
    /// depending on user choice.
    pub fn hax_def(&mut self, item: &hax::ItemRef) -> Result<Arc<hax::FullDef>, Error> {
        let item = if self.monomorphize() {
            RustcItem::Mono(item.clone())
        } else {
            RustcItem::Poly(item.def_id.clone())
        };
        self.t_ctx.hax_def_for_item(&item)
    }

    pub(crate) fn poly_hax_def(&mut self, def_id: &hax::DefId) -> Result<Arc<hax::FullDef>, Error> {
        self.t_ctx.poly_hax_def(def_id)
    }
}

impl<'a> IntoFormatter for &'a TranslateCtx<'_> {
    type C = FmtCtx<'a>;
    fn into_fmt(self) -> Self::C {
        self.translated.into_fmt()
    }
}

impl<'a> IntoFormatter for &'a ItemTransCtx<'_, '_> {
    type C = FmtCtx<'a>;
    fn into_fmt(self) -> Self::C {
        FmtCtx {
            translated: Some(&self.t_ctx.translated),
            generics: self.binding_levels.map_ref(|bl| Cow::Borrowed(&bl.params)),
            locals: None,
            indent_level: 0,
        }
    }
}

impl<'tcx, 'ctx> fmt::Display for TranslateCtx<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.translated.fmt(f)
    }
}
