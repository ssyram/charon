//! Simplify Box implementations when not in raw-boxes mode.
//!
//! When `--raw-boxes` is not enabled, we want builtin `Box<T>` with just one parameter instead
//! of the full `alloc::boxed::Box<T, A>` with allocator parameter.
//!
//! This pass transforms:
//! - `impl<T, A> Drop for alloc::boxed::Box<T, A> where MetaSized<T>, Sized<A>, Allocator<A>`
//! 
//! To:
//! - `impl<T> Drop for Box<T> where MetaSized<T>`

use crate::{
    ast::*,
    transform::TransformCtx,
};

use super::ctx::TransformPass;

pub struct Transform;

impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        if ctx.options.raw_boxes {
            // Don't simplify when raw-boxes is enabled
            return;
        }

        // TODO: Implement proper Box simplification
        // For now, this is disabled to avoid generic inconsistencies
        // The transform needs to properly handle type and trait clause renumbering
    }
}

/// Check if this trait implementation should be simplified
/// This is a simplified heuristic that looks for implementations with 2 type params and 3+ trait clauses
fn should_simplify_box_impl(timpl: &TraitImpl) -> bool {
    // Simple heuristic: if we have 2 type parameters and 3+ trait clauses, this might be a Box<T, A> implementation
    timpl.generics.types.elem_count() == 2 && timpl.generics.trait_clauses.elem_count() >= 3
}

/// Check if this function is a Box Drop method that should be simplified  
fn should_simplify_box_method(fun_decl: &FunDecl) -> bool {
    // Simple heuristic: if function has 2 type parameters and 3+ trait clauses, this might be a Box<T, A> method
    if let ItemKind::TraitImpl { item_name, .. } = &fun_decl.kind {
        item_name.0 == "drop" && 
        fun_decl.signature.generics.types.elem_count() == 2 && 
        fun_decl.signature.generics.trait_clauses.elem_count() >= 3
    } else {
        false
    }
}

/// Transform a Box trait implementation to use simplified generics
fn transform_box_trait_impl(timpl: &mut TraitImpl) {
    // Simplify generics: remove allocator type parameter and constraints
    if timpl.generics.types.elem_count() >= 2 {
        // Keep only the first type parameter (T), remove allocator (A)
        // We need to remove the second type parameter manually
        if let Some(_) = timpl.generics.types.get(TypeVarId::new(1)) {
            timpl.generics.types.remove(TypeVarId::new(1));
        }
        
        // Keep only the first trait constraint (MetaSized<T>)
        if timpl.generics.trait_clauses.elem_count() >= 3 {
            let mut simplified_trait_clauses = Vector::new();
            if let Some(first_trait_clause) = timpl.generics.trait_clauses.get(TraitClauseId::new(0)) {
                simplified_trait_clauses.push(first_trait_clause.clone());
            }
            timpl.generics.trait_clauses = simplified_trait_clauses;
        }
    }
}

/// Transform a Box method to use simplified generics
fn transform_box_method(fun_decl: &mut FunDecl) {
    // Simplify function generics
    if fun_decl.signature.generics.types.elem_count() >= 2 {
        // Keep only the first type parameter (T), remove allocator (A)
        if let Some(_) = fun_decl.signature.generics.types.get(TypeVarId::new(1)) {
            fun_decl.signature.generics.types.remove(TypeVarId::new(1));
        }
        
        // Keep only the first trait constraint (MetaSized<T>)
        if fun_decl.signature.generics.trait_clauses.elem_count() >= 3 {
            let mut simplified_trait_clauses = Vector::new();
            if let Some(first_trait_clause) = fun_decl.signature.generics.trait_clauses.get(TraitClauseId::new(0)) {
                simplified_trait_clauses.push(first_trait_clause.clone());
            }
            fun_decl.signature.generics.trait_clauses = simplified_trait_clauses;
        }
    }
}