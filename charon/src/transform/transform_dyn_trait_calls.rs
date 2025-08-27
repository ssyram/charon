//! Transform method calls on `&dyn Trait` to vtable function pointer calls.
//!
//! This pass converts direct method calls on trait objects into calls through vtable
//! function pointers. For example:
//!
//! ```rust
//! let x: &dyn Trait = &obj;
//! x.method(args);
//! ```
//!
//! is transformed from:
//! ```
//! @0 := call TraitMethod::method(x, args)
//! ```
//!
//! to:
//! ```
//! vtable@9 := ptr_metadata(move (@receiver))              // Extract vtable pointer
//! method_ptr@8 := copy (((*vtable@9).method_check))       // Get method from vtable
//! @0 := (move method_ptr@8)(move (@receiver), move (@args)) // Call through function pointer
//! ```

use crate::{register_error, transform::TransformCtx, ullbc_ast::*};
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
    ctx: &mut TransformCtx,
    span: Span,
    call: &mut Call,
) -> Result<Vec<Statement>, crate::errors::Error> {
    // Check if this is a trait method call
    let FnOperand::Regular(fn_ptr) = &call.func else {
        return Ok(Vec::new()); // Not a regular function call
    };
    
    let FunIdOrTraitMethodRef::Trait(trait_ref, method_name, _) = fn_ptr.func.as_ref() else {
        return Ok(Vec::new()); // Not a trait method call
    };

    // Check if we have a dyn trait object as receiver
    if call.args.is_empty() {
        return Ok(Vec::new()); // No receiver
    }
    
    let receiver_ty = match &call.args[0] {
        Operand::Copy(place) | Operand::Move(place) => place.ty(),
        Operand::Const(const_expr) => &const_expr.ty,
    };
    if !is_dyn_trait_receiver(receiver_ty) {
        return Ok(Vec::new()); // Not a dyn trait receiver
    }

    // This is a method call on a dyn trait object - we need to transform it
    let statements = Vec::new();

    // TODO: This is where we would implement the actual vtable call transformation
    // For now, we'll register an error indicating this feature is not yet implemented
    register_error!(
        ctx,
        span,
        "Dynamic trait method calls are not yet fully implemented. Found call to {}::{} on dyn trait object",
        trait_ref.trait_decl_ref.name(),
        method_name.0
    );

    Ok(statements)
}

pub struct Transform;

impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, body: &mut ExprBody) {
        let mut new_statements_by_block: std::collections::HashMap<BlockId, Vec<Statement>> = std::collections::HashMap::new();
        
        for (block_id, block) in body.body.iter_mut().enumerate() {
            let block_id = BlockId::new(block_id);
            
            // Check terminator for calls
            if let RawTerminator::Call { call, .. } = &mut block.terminator.content {
                match transform_dyn_trait_call(ctx, block.terminator.span, call) {
                    Ok(new_stmts) if !new_stmts.is_empty() => {
                        new_statements_by_block.insert(block_id, new_stmts);
                    },
                    Ok(_) => {}, // No transformation needed
                    Err(_) => {}, // Error already registered
                }
            }
        }
        
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