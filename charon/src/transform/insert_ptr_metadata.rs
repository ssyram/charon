use crate::transform::TransformCtx;
use crate::{transform::ctx::UllbcPass, ullbc_ast::*};
use derive_generic_visitor::*;

#[derive(Visitor)]
struct BodyVisitor<'a, 'b> {
    locals: &'a mut Locals,
    /// Statements to prepend to the statement currently being explored.
    statements: Vec<Statement>,
    span: Span,
    translated: &'b TranslatedCrate,
}

/// A helper trait to compute the metadata for a given place we are to make ref / raw-ptr from.
pub trait PtrMetadataComputable {
    /// Create a local & return the place pointing to it
    fn get_locals_mut(&mut self) -> &mut Locals;
    fn insert_storage_live_stmt(&mut self, local: LocalId);
    fn insert_assn_stmt(&mut self, place: Place, rvalue: Rvalue);
    fn get_translated(&self) -> &TranslatedCrate;
    fn fresh_var(&mut self, name: Option<String>, ty: Ty) -> Place {
        let var = self.get_locals_mut().new_var(name, ty);
        self.insert_storage_live_stmt(var.local_id());
        var
    }
}

impl PtrMetadataComputable for BodyVisitor<'_, '_> {
    fn get_locals_mut(&mut self) -> &mut Locals {
        self.locals
    }

    fn insert_storage_live_stmt(&mut self, local: LocalId) {
        self.statements
            .push(Statement::new(self.span, RawStatement::StorageLive(local)));
    }

    fn insert_assn_stmt(&mut self, place: Place, rvalue: Rvalue) {
        self.statements.push(Statement::new(
            self.span,
            RawStatement::Assign(place, rvalue),
        ));
    }

    fn get_translated(&self) -> &TranslatedCrate {
        self.translated
    }
}

/// Get the outmost deref of a place, if it exists. Returns the place that the deref happens on.
fn outmost_deref(place: &Place) -> Option<&Place> {
    let (subplace, proj) = place.as_projection()?;
    match proj {
        ProjectionElem::Deref => Some(subplace),
        _ => outmost_deref(subplace),
    }
}

fn get_ptr_metadata_aux<T: PtrMetadataComputable>(ctx: &mut T, place: &Place) -> Option<Operand> {
    let place = outmost_deref(place)?;
    let ty = match place.ty().get_ptr_metadata(ctx.get_translated()) {
        PtrMetadata::None => None,
        PtrMetadata::Length => Some(Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize)))),
        PtrMetadata::VTable(type_decl_ref) => Some(Ty::new(TyKind::Ref(
            Region::Static,
            Ty::new(TyKind::Adt(type_decl_ref)),
            RefKind::Shared,
        ))),
        PtrMetadata::InheritFrom(ty) => Some(Ty::new(TyKind::PtrMetadata(ty))),
    }?;
    let new_place = ctx.fresh_var(None, ty);
    // it is `Copy` because `place` is a deref, which means it is a pointer / ref
    ctx.insert_assn_stmt(
        new_place.clone(),
        Rvalue::UnaryOp(UnOp::PtrMetadata, Operand::Copy(place.clone())),
    );
    Some(Operand::Move(new_place))
}

/// When a place is to be referred to as a reference or a raw pointer, we compute the metadata required
/// for this operation and return it as an operand.
/// New locals & statements are to be inserted before the target place to keep the metadata.
pub fn place_ptr_metadata_operand<T: PtrMetadataComputable>(ctx: &mut T, place: &Place) -> Operand {
    match get_ptr_metadata_aux(ctx, place) {
        Some(metadata) => metadata,
        None => Operand::mk_const_unit(), // No metadata, use unit
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
                    translated: &ctx.translated,
                };
                let _ = st.drive_body_mut(&mut visitor);
                visitor.statements
            });
        });
    }
}
