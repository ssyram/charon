use crate::ast::*;
use crate::errors::{ErrorCtx, Level};
use crate::formatter::{FmtCtx, IntoFormatter};
use crate::llbc_ast;
use crate::options::TranslateOptions;
use crate::pretty::FmtWithCtx;
use crate::ullbc_ast;
use std::cell::RefCell;
use std::{fmt, mem};

/// Simpler context used for rustc-independent code transformation. This only depends on rustc for
/// its error reporting machinery.
pub struct TransformCtx {
    /// The options that control transformation.
    pub options: TranslateOptions,
    /// The translated data.
    pub translated: TranslatedCrate,
    /// Context for tracking and reporting errors.
    pub errors: RefCell<ErrorCtx>,
}

/// A pass that modifies ullbc bodies.
pub trait UllbcPass: Sync {
    /// Transform a body.
    fn transform_body(&self, _ctx: &mut TransformCtx, _body: &mut ullbc_ast::ExprBody) {}

    /// Transform a function declaration. This forwards to `transform_body` by default.
    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        if let Ok(body) = &mut decl.body {
            self.transform_body(ctx, body.as_unstructured_mut().unwrap())
        }
    }

    /// Transform the given context. This forwards to the other methods by default.
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|ctx, decl| {
            let body = decl
                .body
                .as_mut()
                .map(|body| body.as_unstructured_mut().unwrap())
                .map_err(|opaque| *opaque);
            self.log_before_body(ctx, &decl.item_meta.name, body.as_deref());
            self.transform_function(ctx, decl);
        });
    }

    /// The name of the pass, used for debug logging. The default implementation uses the type
    /// name.
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    /// Log that the pass is about to be run on this body.
    fn log_before_body(
        &self,
        ctx: &TransformCtx,
        name: &Name,
        body: Result<&ullbc_ast::ExprBody, &Opaque>,
    ) {
        let fmt_ctx = &ctx.into_fmt();
        let body_str = if let Ok(body) = body {
            body.to_string_with_ctx(fmt_ctx)
        } else {
            "<opaque>".to_owned()
        };
        trace!(
            "# About to run pass [{}] on `{}`:\n{}",
            self.name(),
            name.with_ctx(fmt_ctx),
            body_str,
        );
    }
}

/// A pass that modifies llbc bodies.
pub trait LlbcPass: Sync {
    /// Transform a body.
    fn transform_body(&self, _ctx: &mut TransformCtx, _body: &mut llbc_ast::ExprBody) {}

    /// Transform a function declaration. This forwards to `transform_body` by default.
    fn transform_function(&self, ctx: &mut TransformCtx, decl: &mut FunDecl) {
        if let Ok(body) = &mut decl.body {
            self.transform_body(ctx, body.as_structured_mut().unwrap())
        }
    }

    /// Transform the given context. This forwards to the other methods by default.
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        ctx.for_each_fun_decl(|ctx, decl| {
            let body = decl
                .body
                .as_mut()
                .map(|body| body.as_structured_mut().unwrap())
                .map_err(|opaque| *opaque);
            self.log_before_body(ctx, &decl.item_meta.name, body.as_deref());
            self.transform_function(ctx, decl);
        });
    }

    /// The name of the pass, used for debug logging. The default implementation uses the type
    /// name.
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }

    /// Log that the pass is about to be run on this body.
    fn log_before_body(
        &self,
        ctx: &TransformCtx,
        name: &Name,
        body: Result<&llbc_ast::ExprBody, &Opaque>,
    ) {
        let fmt_ctx = &ctx.into_fmt();
        let body_str = if let Ok(body) = body {
            body.to_string_with_ctx(fmt_ctx)
        } else {
            "<opaque>".to_owned()
        };
        trace!(
            "# About to run pass [{}] on `{}`:\n{}",
            self.name(),
            name.with_ctx(fmt_ctx),
            body_str,
        );
    }
}

/// A pass that transforms the crate data.
pub trait TransformPass: Sync {
    fn transform_ctx(&self, ctx: &mut TransformCtx);

    /// The name of the pass, used for debug logging. The default implementation uses the type
    /// name.
    fn name(&self) -> &str {
        std::any::type_name::<Self>()
    }
}

impl<'ctx> TransformCtx {
    pub(crate) fn has_errors(&self) -> bool {
        self.errors.borrow().has_errors()
    }

    /// Span an error and register the error.
    pub(crate) fn span_err(&self, span: Span, msg: &str, level: Level) -> Error {
        self.errors
            .borrow_mut()
            .span_err(&self.translated, span, msg, level)
    }

    pub(crate) fn opacity_for_name(&self, name: &Name) -> ItemOpacity {
        self.options.opacity_for_name(&self.translated, name)
    }

    pub(crate) fn with_def_id<F, T>(
        &mut self,
        def_id: impl Into<AnyTransId>,
        def_id_is_local: bool,
        f: F,
    ) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let mut errors = self.errors.borrow_mut();
        let current_def_id = mem::replace(&mut errors.def_id, Some(def_id.into()));
        let current_def_id_is_local = mem::replace(&mut errors.def_id_is_local, def_id_is_local);
        drop(errors); // important: release the refcell "lock"
        let ret = f(self);
        let mut errors = self.errors.borrow_mut();
        errors.def_id = current_def_id;
        errors.def_id_is_local = current_def_id_is_local;
        ret
    }

    /// Mutably iterate over the bodies.
    /// Warning: we replace each body with `Err(Opaque)` while inspecting it so we can keep access
    /// to the rest of the crate.
    pub(crate) fn for_each_body(&mut self, mut f: impl FnMut(&mut Self, &mut Body)) {
        let fn_ids = self.translated.fun_decls.all_indices();
        for id in fn_ids {
            if let Some(decl) = self.translated.fun_decls.get_mut(id) {
                if let Ok(mut body) = mem::replace(&mut decl.body, Err(Opaque)) {
                    let fun_decl_id = decl.def_id;
                    let is_local = decl.item_meta.is_local;
                    self.with_def_id(fun_decl_id, is_local, |ctx| f(ctx, &mut body));
                    self.translated.fun_decls[id].body = Ok(body);
                }
            }
        }
    }

    /// Mutably iterate over the function declarations.
    /// Warning: each inspected fundecl becomes inaccessible from `ctx` during the course of this function.
    pub(crate) fn for_each_fun_decl(&mut self, mut f: impl FnMut(&mut Self, &mut FunDecl)) {
        let fn_ids = self.translated.fun_decls.all_indices();
        for id in fn_ids {
            if let Some(mut decl) = self.translated.fun_decls.remove(id) {
                let fun_decl_id = decl.def_id;
                let is_local = decl.item_meta.is_local;
                self.with_def_id(fun_decl_id, is_local, |ctx| f(ctx, &mut decl));
                self.translated.fun_decls.set_slot(id, decl);
            }
        }
    }

    /// Iterate mutably over all items, keeping access to `self`. To make this work, we move out
    /// each item before iterating over it.
    pub fn for_each_item_mut(
        &mut self,
        mut f: impl for<'a> FnMut(&'a mut Self, AnyTransItemMut<'a>),
    ) {
        macro_rules! for_each {
            ($vector:ident, $kind:ident) => {
                for id in self.translated.$vector.all_indices() {
                    if let Some(mut decl) = self.translated.$vector.remove(id) {
                        f(self, AnyTransItemMut::$kind(&mut decl));
                        self.translated.$vector.set_slot(id, decl);
                    }
                }
            };
        }
        for_each!(type_decls, Type);
        for_each!(fun_decls, Fun);
        for_each!(global_decls, Global);
        for_each!(trait_decls, TraitDecl);
        for_each!(trait_impls, TraitImpl);
    }
}

impl<'a> IntoFormatter for &'a TransformCtx {
    type C = FmtCtx<'a>;

    fn into_fmt(self) -> Self::C {
        self.translated.into_fmt()
    }
}

impl fmt::Display for TransformCtx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.translated.fmt(f)
    }
}

/// A helper trait to compute the metadata for a given place we are to make ref / raw-ptr from.
pub trait PtrMetadataComputable {
    /// Create a local & return the place pointing to it
    fn get_locals_mut(&mut self) -> &mut Locals;
    fn insert_storage_live_stmt(&mut self, local: LocalId);
    fn insert_assn_stmt(&mut self, place: Place, rvalue: Rvalue);
    fn get_ctx(&self) -> &TransformCtx;
    fn fresh_var(&mut self, name: Option<String>, ty: Ty) -> Place {
        let var = self.get_locals_mut().new_var(name, ty);
        self.insert_storage_live_stmt(var.local_id().unwrap());
        var
    }
}

fn is_last_field_of_ty_decl_id(
    ctx: &TransformCtx,
    type_decl_id: &TypeDeclId,
    field: &FieldId,
) -> bool {
    let type_decl = ctx.translated.type_decls.get(*type_decl_id).unwrap();
    match &type_decl.kind {
        TypeDeclKind::Struct(vector) => vector.slot_count() - 1 == field.index(),
        // `enum` does not have "last field" concept, also, it should not have metadata as per Rust rules
        TypeDeclKind::Enum(..) => false,
        // Same as `enum` above
        TypeDeclKind::Union(..) => false,
        TypeDeclKind::Opaque => panic!(
            "Accessing the field {} of an opaque type {}! Cannot tell whether this is the last field. Please consider translating the opaque type definition by `--include`.",
            field, type_decl_id.with_ctx(&ctx.into_fmt())
        ),
        TypeDeclKind::Alias(ty) => match ty.kind() {
            TyKind::Adt(type_decl_ref) => match &type_decl_ref.id {
                TypeId::Adt(type_decl_id) => {
                    is_last_field_of_ty_decl_id(ctx, type_decl_id, field)
                }
                TypeId::Tuple => type_decl_ref.generics.len() - 1 == field.index(),
                // Builtin types are atoms
                TypeId::Builtin(..) => panic!("Trying to get the last field from a built-in type"),
            },
            _ => panic!("Type \"{}\" does not have field, projection operation invalid!", ty.with_ctx(&ctx.into_fmt())),
        },
        TypeDeclKind::Error(_) => panic!("Accessing the field of an error type!"),
    }
}

fn is_last_field(ctx: &TransformCtx, proj_kind: &FieldProjKind, field: &FieldId) -> bool {
    match proj_kind {
        FieldProjKind::Adt(type_decl_id, _) => {
            is_last_field_of_ty_decl_id(ctx, type_decl_id, field)
        }
        FieldProjKind::Tuple(arity) => arity - 1 == field.index(),
    }
}

/// Get the outmost deref of a place, if it exists. Returns the place that the deref happens upon and the derefed type.
/// Also check if the projection always performs on the last field, otherwise return None,
/// as it should never have metadata if it is not the last field.
fn outmost_deref_at_last_field(ctx: &TransformCtx, place: &Place) -> Option<(Place, Ty)> {
    let (subplace, proj) = place.as_projection()?;
    match proj {
        // *subplace
        // So that `subplace` is a pointer / reference type
        // We will need to keep the derefed type to get the metadata type
        ProjectionElem::Deref => Some((subplace.clone(), place.ty().clone())),
        ProjectionElem::Field(proj_kind, field) if is_last_field(ctx, proj_kind, field) => {
            outmost_deref_at_last_field(ctx, subplace)
        }
        // Otherwise, it could be one of the following cases:
        // 1. It is a field access, but not the last field, so it does not have metadata;
        // 2. It is an index / sub-slice access, which must return a reference,
        //    As we are checking for *outmost*, it means no deref is performed,
        //    so it must remain a reference, which does not have metadata.
        _ => None,
    }
}

fn get_ptr_metadata_aux<T: PtrMetadataComputable>(ctx: &mut T, place: &Place) -> Option<Operand> {
    trace!(
        "getting ptr metadata for place: {}",
        place.with_ctx(&ctx.get_ctx().into_fmt())
    );
    let (place, deref_ty) = outmost_deref_at_last_field(ctx.get_ctx(), place)?;
    trace!(
        "outmost deref place: {}",
        place.with_ctx(&ctx.get_ctx().into_fmt())
    );
    let ty = match deref_ty.get_ptr_metadata(&ctx.get_ctx().translated) {
        PtrMetadata::None => None,
        PtrMetadata::Length => Some(Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize)))),
        PtrMetadata::VTable(type_decl_ref) => Some(Ty::new(TyKind::Ref(
            Region::Static,
            Ty::new(TyKind::Adt(type_decl_ref)),
            RefKind::Shared,
        ))),
        PtrMetadata::InheritFrom(ty) => Some(Ty::new(TyKind::PtrMetadata(ty))),
    }?;
    trace!(
        "computed metadata type: {}",
        ty.with_ctx(&ctx.get_ctx().into_fmt())
    );
    let new_place = ctx.fresh_var(None, ty);
    // it is `Copy` because `place` is a deref, which means it is a pointer / ref
    ctx.insert_assn_stmt(
        new_place.clone(),
        Rvalue::UnaryOp(UnOp::PtrMetadata, Operand::Copy(place.clone())),
    );
    Some(Operand::Move(new_place))
}

/// No metadata, use unit, but as const ADT (for `()`) is not allowed
/// Introduce a new local to hold this
fn no_metadata<T: PtrMetadataComputable>(ctx: &mut T) -> Operand {
    let new_place = ctx.fresh_var(None, Ty::mk_unit());
    ctx.insert_assn_stmt(new_place.clone(), Rvalue::unit_value());
    Operand::Move(new_place)
}

/// When a place is to be referred to as a reference or a raw pointer, we compute the metadata required
/// for this operation and return it as an operand.
/// New locals & statements are to be inserted before the target place to keep the metadata.
pub fn place_ptr_metadata_operand<T: PtrMetadataComputable>(ctx: &mut T, place: &Place) -> Operand {
    // add a shortcut here -- if the type is originally not a type with ptr-metadata, ignore it
    match place.ty().get_ptr_metadata(&ctx.get_ctx().translated) {
        PtrMetadata::None => return no_metadata(ctx),
        _ => match get_ptr_metadata_aux(ctx, place) {
            Some(metadata) => metadata,
            None => no_metadata(ctx),
        },
    }
}
