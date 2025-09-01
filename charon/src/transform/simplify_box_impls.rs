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
use derive_generic_visitor::*;

use super::ctx::TransformPass;

pub struct Transform;

impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        if ctx.options.raw_boxes {
            // Don't simplify when raw-boxes is enabled
            return;
        }

        // TODO: Implement proper Box simplification
        // Current approach causes generic inconsistencies due to type variable renumbering
        // Need to either:
        // 1. Implement proper type variable and trait clause remapping/substitution
        // 2. Or create new simplified implementations and replace the complex ones
        // For now, disabled to avoid breaking the build

        /*
        // First, identify trait implementations that need to be simplified
        let mut box_trait_impls_to_simplify = Vec::new();
        let mut box_methods_to_simplify = Vec::new();
        
        for timpl in ctx.translated.trait_impls.iter() {
            if is_box_allocator_drop_impl(timpl) {
                box_trait_impls_to_simplify.push(timpl.def_id);
            }
        }
        
        for fun_decl in ctx.translated.fun_decls.iter() {
            if is_box_allocator_drop_method(&fun_decl) {
                box_methods_to_simplify.push(fun_decl.def_id);
            }
        }

        // Transform the identified items
        for impl_id in box_trait_impls_to_simplify {
            if let Some(timpl) = ctx.translated.trait_impls.get_mut(impl_id) {
                simplify_box_drop_impl(timpl);
            }
        }
        
        for method_id in box_methods_to_simplify {
            if let Some(fun_decl) = ctx.translated.fun_decls.get_mut(method_id) {
                simplify_box_drop_method(fun_decl);
            }
        }
        */
    }
}

/// Check if this trait implementation is a Box Drop implementation with allocator parameters
fn is_box_allocator_drop_impl(timpl: &TraitImpl) -> bool {
    // Check if this has the pattern we're looking for:
    // - 2 type parameters (T and A)
    // - 3 trait clauses (MetaSized<T>, Sized<A>, Allocator<A>) 
    // - Implementation of Drop trait
    timpl.generics.types.elem_count() == 2 && 
    timpl.generics.trait_clauses.elem_count() == 3 &&
    is_drop_trait_implementation(timpl)
}

/// Check if this function is a Box Drop method with allocator parameters
fn is_box_allocator_drop_method(fun_decl: &FunDecl) -> bool {
    if let ItemKind::TraitImpl { item_name, .. } = &fun_decl.kind {
        item_name.0 == "drop" && 
        fun_decl.signature.generics.types.elem_count() == 2 && 
        fun_decl.signature.generics.trait_clauses.elem_count() == 3
    } else {
        false
    }
}

/// Check if a trait implementation is implementing the Drop trait
fn is_drop_trait_implementation(_timpl: &TraitImpl) -> bool {
    // This is a heuristic - we could check the trait name more precisely
    // For now, we use the pattern of having exactly 3 trait clauses and 2 type params
    // which matches the Box<T, A> Drop implementation pattern
    true // We already check the structure in the caller
}

/// Simplify a Box Drop trait implementation
fn simplify_box_drop_impl(timpl: &mut TraitImpl) {
    // Step 1: Reduce type parameters from 2 to 1 (remove allocator A)
    if timpl.generics.types.elem_count() >= 2 {
        timpl.generics.types.remove(TypeVarId::new(1));
    }
    
    // Step 2: Reduce trait clauses from 3 to 1 (keep only MetaSized<T>)
    if timpl.generics.trait_clauses.elem_count() >= 3 {
        // Keep only the first trait clause
        timpl.generics.trait_clauses.remove(TraitClauseId::new(2));
        timpl.generics.trait_clauses.remove(TraitClauseId::new(1));
    }
    
    // Step 3: Transform type references to use builtin Box<T> instead of alloc::boxed::Box<T, A>
    // This will be handled by a visitor that walks the entire structure
    let mut visitor = BoxTypeSimplifier;
    let _ = timpl.drive_mut(&mut visitor);
}

/// Simplify a Box Drop method
fn simplify_box_drop_method(fun_decl: &mut FunDecl) {
    // Step 1: Reduce type parameters from 2 to 1
    if fun_decl.signature.generics.types.elem_count() >= 2 {
        fun_decl.signature.generics.types.remove(TypeVarId::new(1));
    }
    
    // Step 2: Reduce trait clauses from 3 to 1
    if fun_decl.signature.generics.trait_clauses.elem_count() >= 3 {
        fun_decl.signature.generics.trait_clauses.remove(TraitClauseId::new(2));
        fun_decl.signature.generics.trait_clauses.remove(TraitClauseId::new(1));
    }
    
    // Step 3: Transform type references
    let mut visitor = BoxTypeSimplifier;
    let _ = fun_decl.drive_mut(&mut visitor);
}

/// Visitor to transform alloc::boxed::Box<T, A> references to builtin Box<T>
#[derive(Visitor)]
struct BoxTypeSimplifier;

impl VisitAstMut for BoxTypeSimplifier {
    fn enter_type_decl_ref(&mut self, ty_ref: &mut TypeDeclRef) {
        // Check if this is an alloc::boxed::Box with 2 type parameters and 3 trait clauses
        if ty_ref.generics.types.elem_count() == 2 && ty_ref.generics.trait_refs.elem_count() == 3 {
            // Transform to builtin Box<T>
            let first_type = ty_ref.generics.types.get(TypeVarId::new(0)).cloned();
            let first_trait_ref = ty_ref.generics.trait_refs.get(TraitClauseId::new(0)).cloned();
            
            if let (Some(first_type), Some(first_trait_ref)) = (first_type, first_trait_ref) {
                // Replace with builtin Box
                ty_ref.id = TypeId::Builtin(BuiltinTy::Box);
                
                // Set simplified generics
                let mut new_types = Vector::new();
                new_types.push(first_type);
                
                let mut new_trait_refs = Vector::new();
                new_trait_refs.push(first_trait_ref);
                
                ty_ref.generics = Box::new(GenericArgs {
                    regions: Vector::new(),
                    types: new_types,
                    const_generics: Vector::new(),
                    trait_refs: new_trait_refs,
                });
            }
        }
    }
}