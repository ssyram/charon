//! Transform method calls on `&dyn Trait` to vtable function pointer calls.
//!
//! This pass converts direct method calls on trait objects into calls through vtable
//! function pointers. For example:
//!
//! ```rust,ignore
//! let x: &dyn Trait = &obj;
//! x.method(args);
//! ```
//!
//! is transformed from:
//! ```text
//! @0 := call TraitMethod::method(x, args)
//! ```
//!
//! to:
//! ```text
//! vtable@9 := ptr_metadata(move (@receiver))              // Extract vtable pointer
//! method_ptr@8 := copy (((*vtable@9).method_check))       // Get method from vtable
//! @0 := (move method_ptr@8)(move (@receiver), move (@args)) // Call through function pointer
//! ```

use super::ctx::UllbcPass;
use crate::{errors::Error, formatter::IntoFormatter, pretty::FmtWithCtx, raise_error, register_error, transform::TransformCtx, ullbc_ast::*};

/// Detect if a call should be transformed to use vtable dispatch
/// Returns the trait reference and method name for the dyn trait call if found
fn detect_dyn_trait_call(call: &Call) -> Option<(TraitRef, TraitItemName)> {
    // Check if this is a regular function call
    let FnOperand::Regular(fn_ptr) = &call.func else {
        return None; // Not a regular function call
    };

    let FunIdOrTraitMethodRef::Trait(trait_ref, method_name, _) = &fn_ptr.func else {
        return None; // Not a trait method call
    };
    
    match &trait_ref.kind {
        TraitRefKind::Dyn => Some((trait_ref.clone(), method_name.clone())),
        _ => None,
    }
}

/// Get the correct field index for a method in the vtable struct.
/// Returns None if the method index cannot be determined.
fn get_method_field_index(
    ctx: &TransformCtx,
    span: Span,
    trait_ref: &TraitRef,
    method_name: &TraitItemName,
) -> Result<FieldId, Error> {
    let trait_name = trait_ref.trait_decl_ref.skip_binder.id.with_ctx(&ctx.into_fmt()).to_string();

    // Try to find the trait declaration
    let Some(trait_decl) = ctx
        .translated
        .trait_decls
        .get(trait_ref.trait_decl_ref.skip_binder.id) else {
        raise_error!(
            ctx,
            span,
            "Trait definition for {} not found!",
            trait_name
        );
    };

    let Some(TypeDeclRef { id: TypeId::Adt(vtable_id), .. }) = trait_decl.vtable else {
        raise_error!(
            ctx,
            span,
            "Vtable for trait {} is None, meaning the trait is non-dyn-compatible!",
            trait_name
        );
    };

    let Some(TypeDecl { kind: TypeDeclKind::Struct(fields), .. }) = ctx.translated.type_decls.get(vtable_id) else {
        raise_error!(
            ctx,
            span,
            "Vtable struct for trait {} is not found when searching for vtable index!",
            trait_name
        );
    };

    // Find the index from the fields
    for (index, field) in fields.iter().enumerate() {
        if format!("method_{}", method_name) == *field.name.as_ref().unwrap() {
            return Ok(FieldId::new(index));
        }
    }

    raise_error!(
        ctx,
        span,
        "Could not determine method index for {} in vtable of trait {}",
        method_name,
        trait_name
    );
}

/// Create local variables needed for vtable dispatch
fn create_vtable_locals(span: Span, statements: &mut Vec<Statement>, locals: &mut Locals, call: &Call) -> (Place, Place) {
    // Create vtable type - for now use a raw pointer as placeholder
    // In complete implementation this would be the actual vtable struct type
    let vtable_ty = Ty::new(TyKind::RawPtr(
        Ty::mk_unit(), // Placeholder vtable pointee
        RefKind::Shared,
    ));

    // Create method pointer type - placeholder function pointer type
    let method_ptr_ty = Ty::new(TyKind::FnPtr(RegionBinder::empty((
        call.args
            .iter()
            .map(|arg| match arg {
                Operand::Copy(place) | Operand::Move(place) => place.ty.clone(),
                Operand::Const(const_expr) => const_expr.ty.clone(),
            })
            .collect(),
        call.dest.ty.clone(),
    ))));

    let vtable_place = locals.new_var(Some("vtable".to_string()), vtable_ty);
    // push the storage-live statements for the new locals
    statements.push(Statement {
        span,
        content: RawStatement::StorageLive(vtable_place.as_local().unwrap()),
        comments_before: vec![],
    });

    let method_ptr_place = locals.new_var(Some("method_ptr".to_string()), method_ptr_ty);
    statements.push(Statement {
        span,
        content: RawStatement::StorageLive(method_ptr_place.as_local().unwrap()),
        comments_before: vec![],
    });

    (vtable_place, method_ptr_place)
}

/// Generate the statement that extracts the vtable pointer from the dyn trait object
fn generate_vtable_extraction(
    span: Span,
    vtable_place: &Place,
    dyn_trait_operand: &Operand,
) -> Statement {
    Statement {
        span,
        content: RawStatement::Assign(
            vtable_place.clone(),
            Rvalue::UnaryOp(UnOp::PtrMetadata, dyn_trait_operand.clone()),
        ),
        comments_before: vec!["Extract vtable pointer from dyn trait object".to_string()],
    }
}

/// Generate the statement that extracts the method pointer from the vtable
fn generate_method_pointer_extraction(
    span: Span,
    method_ptr_place: &Place,
    vtable_place: &Place,
    field_id: FieldId,
) -> Statement {
    // Create vtable dereference: *vtable
    let vtable_deref_place = Place {
        kind: PlaceKind::Projection(Box::new(vtable_place.clone()), ProjectionElem::Deref),
        ty: Ty::mk_unit(), // Placeholder - should be the vtable struct type
    };

    // Create method field projection: (*vtable).method_field
    let method_field_place = Place {
        kind: PlaceKind::Projection(
            Box::new(vtable_deref_place),
            ProjectionElem::Field(
                FieldProjKind::Adt(TypeDeclId::new(0), None), // Placeholder vtable type ID
                field_id,
            ),
        ),
        ty: method_ptr_place.ty.clone(),
    };

    Statement {
        span,
        content: RawStatement::Assign(
            method_ptr_place.clone(),
            Rvalue::Use(Operand::Copy(method_field_place)),
        ),
        comments_before: vec!["Get method pointer from vtable".to_string()],
    }
}

/// Transform a call to a trait method on a dyn trait object
fn transform_dyn_trait_call(
    ctx: &TransformCtx,
    span: Span,
    statements: &mut Vec<Statement>,
    locals: &mut Locals,
    call: &mut Call,
) -> Result<Option<()>, Error> {
    // 1. Detect if this call should be transformed
    let (trait_ref, method_name) = match detect_dyn_trait_call(call) {
        Some(info) => info,
        None => return Ok(None),
    };

    // 2. Get the dyn trait argument
    if call.args.is_empty() {
        raise_error!(ctx, span, "Dyn trait call has no arguments!");
    }
    let dyn_trait_operand = match &call.args[0] {
        Operand::Copy(place) => Operand::Copy(place.clone()),
        Operand::Move(place) => Operand::Move(place.clone()),
        Operand::Const(_) => {
            // Constants should not be dyn trait objects in practice
            return Ok(None);
        }
    };

    // 3. Create local variables for vtable and method pointer
    let (vtable_place, method_ptr_place) = create_vtable_locals(span, statements, locals, call);

    // 4. Get the correct field index for the method
    let field_id = get_method_field_index(ctx, span, &trait_ref, &method_name)?;

    // Extract vtable pointer
    statements.push(generate_vtable_extraction(
        span,
        &vtable_place,
        &dyn_trait_operand,
    ));

    // Extract method pointer from vtable
    statements.push(generate_method_pointer_extraction(
        span,
        &method_ptr_place,
        &vtable_place,
        field_id,
    ));

    // 6. Transform the original call to use the function pointer
    call.func = FnOperand::Move(method_ptr_place);

    trace!(
        "Generated {} new statements for vtable dispatch",
        statements.len()
    );
    Ok(Some(()))
}

pub struct Transform;

impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, body: &mut ExprBody) {
        trace!(
            "TransformDynTraitCalls: Processing body with {} blocks",
            body.body.iter().count()
        );

        for (block_id, block) in body.body.iter_mut().enumerate() {
            let block_id = BlockId::new(block_id);

            // Check terminator for calls
            if let RawTerminator::Call { call, .. } = &mut block.terminator.content {
                trace!("Found call in block {}: {:?}", block_id, call.func);
                let span = block.terminator.span;
                match transform_dyn_trait_call(ctx, block.terminator.span, &mut block.statements, &mut body.locals, call) {
                    Ok(Some(_)) => {
                        trace!("Successfully transformed dynamic trait call");
                    }
                    Ok(None) => {
                        trace!("No transformation needed for this call");
                    }
                    Err(e) => {
                        register_error!(ctx, span, "Failed to transform dynamic trait call: {:?}", e);
                    }
                }
            }
        }
    }

    fn name(&self) -> &str {
        "TransformDynTraitCalls"
    }
}
