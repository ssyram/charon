use crate::formatter::IntoFormatter;
use crate::pretty::FmtWithCtx;
use crate::transform::TransformCtx;
use crate::transform::ctx::{BodyTransformCtx};
use crate::{transform::ctx::UllbcPass, ullbc_ast::*};
use derive_generic_visitor::*;

#[derive(Visitor)]
struct BodyVisitor<'a, 'b> {
    locals: &'a mut Locals,
    /// Statements to prepend to the statement currently being explored.
    statements: Vec<Statement>,
    span: Span,
    ctx: &'b TransformCtx,
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
        TypeDeclKind::Alias(ty) => panic!("Alias type {} should have been resolved before this point! Found alias to {}.", type_decl_id.with_ctx(&ctx.into_fmt()), ty.with_ctx(&ctx.into_fmt())),
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

fn get_ptr_metadata_aux<T: BodyTransformCtx>(ctx: &mut T, place: &Place) -> Option<Operand> {
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
fn no_metadata<T: BodyTransformCtx>(ctx: &mut T) -> Operand {
    let new_place = ctx.fresh_var(None, Ty::mk_unit());
    ctx.insert_assn_stmt(new_place.clone(), Rvalue::unit_value());
    Operand::Move(new_place)
}

/// When a place is to be referred to as a reference or a raw pointer, we compute the metadata required
/// for this operation and return it as an operand.
/// New locals & statements are to be inserted before the target place to keep the metadata.
pub fn place_ptr_metadata_operand<T: BodyTransformCtx>(ctx: &mut T, place: &Place) -> Operand {
    // add a shortcut here -- if the type is originally not a type with ptr-metadata, ignore it
    match place.ty().get_ptr_metadata(&ctx.get_ctx().translated) {
        PtrMetadata::None => return no_metadata(ctx),
        _ => match get_ptr_metadata_aux(ctx, place) {
            Some(metadata) => metadata,
            None => no_metadata(ctx),
        },
    }
}


impl BodyTransformCtx for BodyVisitor<'_, '_> {
    fn get_locals_mut(&mut self) -> &mut Locals {
        self.locals
    }

    fn insert_storage_live_stmt(&mut self, local: LocalId) {
        self.statements
            .push(Statement::new(self.span, StatementKind::StorageLive(local)));
    }

    fn insert_assn_stmt(&mut self, place: Place, rvalue: Rvalue) {
        self.statements.push(Statement::new(
            self.span,
            StatementKind::Assign(place, rvalue),
        ));
    }

    fn get_ctx(&self) -> &TransformCtx {
        self.ctx
    }
}

impl VisitBodyMut for BodyVisitor<'_, '_> {
    fn visit_rvalue(&mut self, x: &mut Rvalue) -> ::std::ops::ControlFlow<Self::Break> {
        match x {
            Rvalue::Ref { place, kind, .. } => {
                let metadata = place_ptr_metadata_operand(self, &place);
                *x = Rvalue::Ref {
                    place: place.clone(),
                    kind: *kind,
                    ptr_metadata: metadata,
                };
            }
            Rvalue::RawPtr { place, kind, .. } => {
                let metadata = place_ptr_metadata_operand(self, &place);
                *x = Rvalue::RawPtr {
                    place: place.clone(),
                    kind: *kind,
                    ptr_metadata: metadata,
                };
            }
            _ => {}
        }
        Continue(())
    }
}

pub struct Transform;

/// This pass computes the metadata for Rvalue, which is used to create references and raw pointers.
/// E.g., in cases like:
/// ```ignore
/// let x = &[mut] (*some_v).field;
/// ```
/// If the `(*some_v).field` is a DST, like `[i32]`, we will need to fetch the metadata, i.e., the length of the slice,
/// and store it in a local variable, then we have:
/// ```ignore
/// let x = Rvalue::Ref { place:(*some_v).field, kind: [mut], ptr_metadata: PtrMetadata(some_v) };
/// ```
/// There should be a new local variable introduced to store `PtrMetadata(some_v)`.
impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, b: &mut ExprBody) {
        b.body.iter_mut().for_each(|data| {
            data.transform(|st: &mut Statement| {
                let mut visitor = BodyVisitor {
                    locals: &mut b.locals,
                    statements: Vec::new(),
                    span: st.span,
                    ctx: &ctx,
                };
                let _ = st.drive_body_mut(&mut visitor);
                visitor.statements
            });
        });
    }
}
