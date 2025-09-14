use crate::transform::TransformCtx;
use crate::transform::ctx::{place_ptr_metadata_operand, PtrMetadataComputable};
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

impl PtrMetadataComputable for BodyVisitor<'_, '_> {
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
