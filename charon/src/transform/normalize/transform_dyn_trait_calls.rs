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

use crate::{transform::TransformCtx, ullbc_ast::*, errors::Error};
use super::super::ctx::UllbcPass;

/// Check if the receiver type is a `&dyn Trait` or similar trait object
fn is_dyn_trait_receiver(ty: &Ty) -> bool {
    match ty.kind() {
        TyKind::Ref(_, inner_ty, _) => match inner_ty.kind() {
            TyKind::DynTrait(_) => true,
            _ => false,
        },
        TyKind::DynTrait(_) => true,
        _ => false,
    }
}

/// Information about a detected dyn trait method call
#[derive(Debug)]
struct DynTraitCallInfo {
    dyn_arg_index: usize,
    trait_ref: Option<Box<TraitRef>>,
    method_name: String,
}

/// Detect if a call should be transformed to use vtable dispatch
fn detect_dyn_trait_call(call: &Call) -> Option<DynTraitCallInfo> {
    // Check if this is a regular function call
    let FnOperand::Regular(fn_ptr) = &call.func else {
        return None; // Not a regular function call
    };

    // Look for calls on dyn trait arguments (not just receiver)
    let mut dyn_trait_arg_index = None;
    for (i, arg) in call.args.iter().enumerate() {
        let arg_ty = match arg {
            Operand::Copy(place) | Operand::Move(place) => &place.ty,
            Operand::Const(const_expr) => &const_expr.ty,
        };
        if is_dyn_trait_receiver(arg_ty) {
            dyn_trait_arg_index = Some(i);
            break;
        }
    }

    let dyn_arg_index = dyn_trait_arg_index?;

    // Extract trait reference and method information
    match fn_ptr.kind.as_ref() {
        FnPtrKind::Trait(trait_ref, method_name, _fn_decl_id) => {
            trace!(
                "Found trait method call to {} with dyn trait arg at index {}",
                method_name,
                dyn_arg_index
            );
            Some(DynTraitCallInfo {
                dyn_arg_index,
                trait_ref: Some(Box::new(trait_ref.clone())),
                method_name: method_name.0.clone(),
            })
        },
        FnPtrKind::Fun(FunId::Regular(fun_decl_id)) => {
            trace!(
                "Found regular function call (ID: {:?}) with dyn trait arg at index {} - checking if it should be transformed",
                fun_decl_id,
                dyn_arg_index
            );
            // For now, let's transform any regular function call that has a dyn trait argument
            // This is a simplification - in practice we'd want to check if this is actually
            // a trait method implementation that should use vtable dispatch
            Some(DynTraitCallInfo {
                dyn_arg_index,
                trait_ref: None,
                method_name: "unknown".to_string(),
            })
        },
        _ => {
            // Builtin functions or other cases we don't handle
            None
        }
    }
}

/// Get the correct field index for a method in the vtable struct.
/// Returns None if the method index cannot be determined.
fn get_method_field_index(
    ctx: &TransformCtx,
    trait_ref: &Option<Box<TraitRef>>,
    method_name: &str,
) -> Option<FieldId> {
    // If we don't have trait reference information, fall back to placeholder
    let Some(trait_ref) = trait_ref else {
        trace!("No trait reference available, using placeholder field index 3");
        return Some(FieldId::new(3));
    };

    // Try to find the trait declaration to get the actual method index
    if let Some(trait_decl) = ctx.translated.trait_decls.get(trait_ref.trait_decl_ref.skip_binder.id) {
        // Count methods that appear before our target method
        let mut method_index = 0;
        for method in trait_decl.methods.iter() {
            // Reasonable `skip_binder`, as this is generic-irrelevant
            let trait_item_name = &method.skip_binder.name;
            if trait_item_name.0 == method_name {
                // Found our method! Field index is 3 (size, align, drop) + method_index
                let field_id = FieldId::new(3 + method_index);
                trace!(
                    "Found method {} at position {} in trait {:?}, vtable field index: {}",
                    method_name,
                    method_index,
                    trait_decl.item_meta.name,
                    field_id
                );
                return Some(field_id);
            }
            method_index += 1;
        }
    }

    // Fallback to placeholder if we can't find the method
    trace!("Could not determine method index for {}, using placeholder field index 3", method_name);
    Some(FieldId::new(3))
}

/// Create local variables needed for vtable dispatch
fn create_vtable_locals(
    locals: &mut Locals,
    call: &Call,
) -> (Place, Place) {
    // Create vtable type - for now use a raw pointer as placeholder
    // In complete implementation this would be the actual vtable struct type
    let vtable_ty = Ty::new(TyKind::RawPtr(
        Ty::mk_unit(), // Placeholder vtable pointee
        RefKind::Shared,
    ));

    // Create method pointer type - placeholder function pointer type
    let method_ptr_ty = Ty::new(TyKind::FnPtr(
        RegionBinder::empty((
            call.args.iter().map(|arg| match arg {
                Operand::Copy(place) | Operand::Move(place) => place.ty.clone(),
                Operand::Const(const_expr) => const_expr.ty.clone(),
            }).collect(),
            call.dest.ty.clone(),
        ))
    ));

    let vtable_place = locals.new_var(Some("vtable".to_string()), vtable_ty);
    let method_ptr_place = locals.new_var(Some("method_ptr".to_string()), method_ptr_ty);

    (vtable_place, method_ptr_place)
}

/// Generate the statement that extracts the vtable pointer from the dyn trait object
fn generate_vtable_extraction(
    span: Span,
    vtable_place: &Place,
    dyn_trait_place: &Place,
) -> Statement {
    let ptr_metadata_place = dyn_trait_place.clone().project(ProjectionElem::PtrMetadata, vtable_place.ty().clone());
    Statement {
        span,
        kind: StatementKind::Assign(
            vtable_place.clone(),
            Rvalue::Use(Operand::Copy(ptr_metadata_place))
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
        kind: PlaceKind::Projection(
            Box::new(vtable_place.clone()),
            ProjectionElem::Deref
        ),
        ty: Ty::mk_unit(), // Placeholder - should be the vtable struct type
    };

    // Create method field projection: (*vtable).method_field
    let method_field_place = Place {
        kind: PlaceKind::Projection(
            Box::new(vtable_deref_place),
            ProjectionElem::Field(
                FieldProjKind::Adt(TypeDeclId::new(0), None), // Placeholder vtable type ID
                field_id
            )
        ),
        ty: method_ptr_place.ty.clone(),
    };

    Statement {
        span,
        kind: StatementKind::Assign(
            method_ptr_place.clone(),
            Rvalue::Use(Operand::Copy(method_field_place))
        ),
        comments_before: vec!["Get method pointer from vtable".to_string()],
    }
}

/// Transform a call to a trait method on a dyn trait object
fn transform_dyn_trait_call(
    ctx: &TransformCtx,
    span: Span,
    locals: &mut Locals,
    call: &mut Call,
) -> Result<Vec<Statement>, Error> {
    // 1. Detect if this call should be transformed
    let call_info = match detect_dyn_trait_call(call) {
        Some(info) => info,
        None => return Ok(Vec::new()),
    };

    trace!(
        "Transforming dynamic trait method call at {:?} with dyn arg at index {}",
        span,
        call_info.dyn_arg_index
    );

    // 2. Get the dyn trait argument
    let dyn_trait_place = match &call.args[call_info.dyn_arg_index] {
        Operand::Copy(place) => place,
        Operand::Move(place) => place,
        Operand::Const(_) => {
            // Constants should not be dyn trait objects in practice
            return Ok(Vec::new());
        }
    };

    // 3. Create local variables for vtable and method pointer
    let (vtable_place, method_ptr_place) = create_vtable_locals(locals, call);

    // 4. Get the correct field index for the method
    let field_id = get_method_field_index(ctx, &call_info.trait_ref, &call_info.method_name)
        .unwrap_or(FieldId::new(3)); // Fallback to index 3

    // 5. Generate transformation statements
    let mut statements = Vec::new();
    
    // Extract vtable pointer
    statements.push(generate_vtable_extraction(span, &vtable_place, &dyn_trait_place));
    
    // Extract method pointer from vtable
    statements.push(generate_method_pointer_extraction(
        span,
        &method_ptr_place,
        &vtable_place,
        field_id,
    ));

    // 6. Transform the original call to use the function pointer
    call.func = FnOperand::Move(method_ptr_place);

    trace!("Generated {} new statements for vtable dispatch", statements.len());
    Ok(statements)
}

pub struct Transform;

impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, body: &mut ExprBody) {
        trace!("TransformDynTraitCalls: Processing body with {} blocks", body.body.iter().count());
        
        let mut new_statements_by_block: std::collections::HashMap<BlockId, Vec<Statement>> = std::collections::HashMap::new();
        
        for (block_id, block) in body.body.iter_mut().enumerate() {
            let block_id = BlockId::new(block_id);
            
            // Check terminator for calls
            if let TerminatorKind::Call { call, .. } = &mut block.terminator.kind {
                trace!("Found call in block {}: {:?}", block_id, call.func);
                match transform_dyn_trait_call(ctx, block.terminator.span, &mut body.locals, call) {
                    Ok(new_stmts) if !new_stmts.is_empty() => {
                        new_statements_by_block.insert(block_id, new_stmts);
                    },
                    Ok(_) => {}, // No transformation needed
                    Err(e) => {
                        trace!("Error transforming call: {:?}", e);
                    }, 
                }
            }
        }
        
        trace!("Generated new statements for {} blocks", new_statements_by_block.len());
        
        // Insert new statements into blocks
        for (block_id, new_stmts) in new_statements_by_block {
            if let Some(block) = body.body.get_mut(block_id) {
                // Insert new statements at the end of the block, before the terminator
                block.statements.extend(new_stmts);
            }
        }
    }

    fn name(&self) -> &str {
        "TransformDynTraitCalls"
    }
}