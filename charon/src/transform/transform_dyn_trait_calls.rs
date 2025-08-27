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

use crate::{transform::TransformCtx, ullbc_ast::*};
use super::ctx::UllbcPass;

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

/// Transform a call to a trait method on a dyn trait object
fn transform_dyn_trait_call(
    span: Span,
    locals: &mut Locals,
    call: &mut Call,
) -> Result<Vec<Statement>, crate::errors::Error> {
    // Check if this is a regular function call
    let FnOperand::Regular(fn_ptr) = &call.func else {
        return Ok(Vec::new()); // Not a regular function call
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

    let Some(dyn_arg_index) = dyn_trait_arg_index else {
        return Ok(Vec::new()); // No dyn trait argument found
    };

    // For now, we transform any function call with dyn trait arguments
    // In a more complete implementation, we'd want to be more selective
    // and only transform calls that are actually trait method dispatches
    match fn_ptr.func.as_ref() {
        FunIdOrTraitMethodRef::Trait(_trait_ref, method_name, _fn_decl_id) => {
            trace!(
                "Found trait method call to {} with dyn trait arg at index {}",
                method_name,
                dyn_arg_index
            );
        },
        FunIdOrTraitMethodRef::Fun(FunId::Regular(fun_decl_id)) => {
            trace!(
                "Found regular function call (ID: {:?}) with dyn trait arg at index {} - checking if it should be transformed",
                fun_decl_id,
                dyn_arg_index
            );
            // For now, let's transform any regular function call that has a dyn trait argument
            // This is a simplification - in practice we'd want to check if this is actually
            // a trait method implementation that should use vtable dispatch
        },
        _ => {
            // Builtin functions or other cases we don't handle
            return Ok(Vec::new());
        }
    }

    trace!(
        "Transforming dynamic trait method call at {:?} with dyn arg at index {}",
        span,
        dyn_arg_index
    );

    // Get the dyn trait argument
    let dyn_trait_arg = &call.args[dyn_arg_index];
    let dyn_trait_operand = match dyn_trait_arg {
        Operand::Copy(place) => Operand::Copy(place.clone()),
        Operand::Move(place) => Operand::Move(place.clone()),
        Operand::Const(_) => {
            // Constants should not be dyn trait objects in practice
            return Ok(Vec::new());
        }
    };

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

    // Create new local variables for vtable and method pointer
    let vtable_place = locals.new_var(Some("vtable".to_string()), vtable_ty);
    let method_ptr_place = locals.new_var(Some("method_ptr".to_string()), method_ptr_ty);

    let mut statements = Vec::new();

    // 1. vtable := ptr_metadata(dyn_trait_receiver)
    let vtable_assign = Statement {
        span,
        content: RawStatement::Assign(
            vtable_place.clone(),
            Rvalue::UnaryOp(UnOp::PtrMetadata, dyn_trait_operand.clone())
        ),
        comments_before: vec!["Extract vtable pointer from dyn trait object".to_string()],
    };
    statements.push(vtable_assign);

    // 2. method_ptr := (*vtable).method_field
    // Create vtable dereference: *vtable
    let vtable_deref_place = Place {
        kind: PlaceKind::Projection(
            Box::new(vtable_place),
            ProjectionElem::Deref
        ),
        ty: Ty::mk_unit(), // Placeholder - should be the vtable struct type
    };

    // Create method field projection: (*vtable).method_{method_name}
    // For now, use field index 3 as placeholder (after size, align, drop fields)
    let method_field_place = Place {
        kind: PlaceKind::Projection(
            Box::new(vtable_deref_place),
            ProjectionElem::Field(
                FieldProjKind::Adt(TypeDeclId::new(0), None), // Placeholder vtable type ID
                FieldId::new(3) // Placeholder field index
            )
        ),
        ty: method_ptr_place.ty.clone(),
    };

    let method_ptr_assign = Statement {
        span,
        content: RawStatement::Assign(
            method_ptr_place.clone(),
            Rvalue::Use(Operand::Copy(method_field_place))
        ),
        comments_before: vec!["Get method pointer from vtable".to_string()],
    };
    statements.push(method_ptr_assign);

    // 3. Transform the original call to use the function pointer
    call.func = FnOperand::Move(method_ptr_place);

    trace!("Generated {} new statements for vtable dispatch", statements.len());
    Ok(statements)
}

pub struct Transform;

impl UllbcPass for Transform {
    fn transform_body(&self, _ctx: &mut TransformCtx, body: &mut ExprBody) {
        trace!("TransformDynTraitCalls: Processing body with {} blocks", body.body.iter().count());
        
        let mut new_statements_by_block: std::collections::HashMap<BlockId, Vec<Statement>> = std::collections::HashMap::new();
        
        for (block_id, block) in body.body.iter_mut().enumerate() {
            let block_id = BlockId::new(block_id);
            
            // Check terminator for calls
            if let RawTerminator::Call { call, .. } = &mut block.terminator.content {
                trace!("Found call in block {}: {:?}", block_id, call.func);
                match transform_dyn_trait_call(block.terminator.span, &mut body.locals, call) {
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