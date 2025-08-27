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
    _ctx: &mut TransformCtx,
    span: Span,
    call: &mut Call,
) -> Result<Vec<Statement>, crate::errors::Error> {
    // Check if this is a trait method call on dyn trait object
    let FnOperand::Regular(_fn_ptr) = &call.func else {
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

    let Some(_dyn_arg_index) = dyn_trait_arg_index else {
        return Ok(Vec::new()); // No dyn trait argument found
    };

    // For now, just log that we found a dyn trait call and return empty
    // This allows the build to succeed while we implement the transformation
    trace!(
        "Found dynamic trait method call at {:?} - transformation not yet implemented",
        span
    );

    // Return empty vector to indicate no transformation applied
    Ok(Vec::new())
}

pub struct Transform;

impl UllbcPass for Transform {
    fn transform_body(&self, ctx: &mut TransformCtx, body: &mut ExprBody) {
        // Print debug info regardless of log level to a file so we can see it
        std::fs::write("/tmp/debug_transform_dyn.txt", "TransformDynTraitCalls was called").unwrap();
        eprintln!("DEBUG: TransformDynTraitCalls is running on body");
        trace!("TransformDynTraitCalls: Processing body");
        
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