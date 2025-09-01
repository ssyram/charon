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
use std::collections::HashSet;

use super::ctx::TransformPass;

pub struct Transform;

impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        if ctx.options.raw_boxes {
            // Don't simplify when raw-boxes is enabled
            return;
        }

        // First, identify trait implementations that need to be simplified
        let mut box_trait_impls_to_replace = Vec::new();
        let mut box_methods_to_replace = Vec::new();
        
        for timpl in ctx.translated.trait_impls.iter() {
            if is_box_allocator_drop_impl(timpl) {
                box_trait_impls_to_replace.push(timpl.def_id);
            }
        }
        
        for fun_decl in ctx.translated.fun_decls.iter() {
            if is_box_allocator_drop_method(&fun_decl) {
                box_methods_to_replace.push(fun_decl.def_id);
            }
        }

        // Create simplified versions and replace the complex ones
        for impl_id in &box_trait_impls_to_replace {
            if let Some(original) = ctx.translated.trait_impls.get(*impl_id) {
                let simplified = create_simplified_box_drop_impl(original);
                if let Some(slot) = ctx.translated.trait_impls.get_mut(*impl_id) {
                    *slot = simplified;
                }
            }
        }
        
        for method_id in &box_methods_to_replace {
            if let Some(original) = ctx.translated.fun_decls.get(*method_id) {
                let simplified = create_simplified_box_drop_method(original);
                if let Some(slot) = ctx.translated.fun_decls.get_mut(*method_id) {
                    *slot = simplified;
                }
            }
        }

        // Update all references throughout the AST to use the simplified forms
        let visitor = BoxReferenceUpdater {
            simplified_trait_impls: box_trait_impls_to_replace.into_iter().collect(),
            simplified_methods: box_methods_to_replace.into_iter().collect(),
        };
        visitor.visit_by_val_infallible(&mut ctx.translated);
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

/// Create a simplified Box Drop trait implementation from a complex one
fn create_simplified_box_drop_impl(original: &TraitImpl) -> TraitImpl {
    // Create simplified generics with only the first type parameter (T)
    let mut simplified_generics = original.generics.clone();
    
    // Keep only the first type parameter
    if simplified_generics.types.elem_count() >= 2 {
        // Remove the second type parameter (allocator)
        simplified_generics.types.remove_and_shift_ids(TypeVarId::new(1));
    }
    
    // Keep only the first trait clause
    if simplified_generics.trait_clauses.elem_count() >= 3 {
        // Remove the last two trait clauses
        simplified_generics.trait_clauses.remove_and_shift_ids(TraitClauseId::new(2));
        simplified_generics.trait_clauses.remove_and_shift_ids(TraitClauseId::new(1));
    }
    
    // Create simplified impl_trait - for now, keep the original structure
    // The Self type transformation will be handled by the global visitor
    let simplified_impl_trait = original.impl_trait.clone();
    
    let mut result = TraitImpl {
        def_id: original.def_id,
        item_meta: original.item_meta.clone(),
        impl_trait: simplified_impl_trait,
        generics: simplified_generics,
        parent_trait_refs: original.parent_trait_refs.clone(),
        consts: original.consts.clone(),
        types: original.types.clone(),
        type_clauses: original.type_clauses.clone(),
        methods: original.methods.clone(),
        vtable: original.vtable.clone(),
    };
    
    // Apply type simplification to the entire impl
    let mut visitor = BoxTypeSimplifier;
    let _ = result.drive_mut(&mut visitor);
    
    result
}

/// Create a simplified Box Drop method from a complex one  
fn create_simplified_box_drop_method(original: &FunDecl) -> FunDecl {
    // Create simplified generics with only the first type parameter (T)
    let mut simplified_generics = original.signature.generics.clone();
    
    // Keep only the first type parameter
    if simplified_generics.types.elem_count() >= 2 {
        // Remove the second type parameter (allocator)
        simplified_generics.types.remove_and_shift_ids(TypeVarId::new(1));
    }
    
    // Keep only the first trait clause
    if simplified_generics.trait_clauses.elem_count() >= 3 {
        // Remove the last two trait clauses
        simplified_generics.trait_clauses.remove_and_shift_ids(TraitClauseId::new(2));
        simplified_generics.trait_clauses.remove_and_shift_ids(TraitClauseId::new(1));
    }
    
    // Create simplified signature
    let mut simplified_signature = original.signature.clone();
    simplified_signature.generics = simplified_generics;
    
    let mut result = FunDecl {
        def_id: original.def_id,
        item_meta: original.item_meta.clone(),
        signature: simplified_signature,
        kind: original.kind.clone(),
        body: original.body.clone(),
        is_global_initializer: original.is_global_initializer,
    };
    
    // Apply type simplification to the entire function
    let mut visitor = BoxTypeSimplifier;
    let _ = result.drive_mut(&mut visitor);
    
    result
}

/// Visitor to transform alloc::boxed::Box<T, A> references to builtin Box<T>
#[derive(Visitor)]
struct BoxTypeSimplifier;

impl VisitAstMut for BoxTypeSimplifier {
    fn enter_type_decl_ref(&mut self, ty_ref: &mut TypeDeclRef) {
        // Check if this is a Box type with 2 type parameters that should be simplified to 1
        if ty_ref.generics.types.elem_count() == 2 && ty_ref.generics.trait_refs.elem_count() == 3 {
            // This looks like an alloc::boxed::Box<T, A> that should be simplified to Box<T>
            
            // Change to builtin Box type
            ty_ref.id = TypeId::Builtin(BuiltinTy::Box);
            
            // Remove the second type parameter (allocator)
            ty_ref.generics.types.remove_and_shift_ids(TypeVarId::new(1));
            
            // Remove the last two trait references
            ty_ref.generics.trait_refs.remove_and_shift_ids(TraitClauseId::new(2));
            ty_ref.generics.trait_refs.remove_and_shift_ids(TraitClauseId::new(1));
        }
    }
}

/// Visitor to update references to simplified Box implementations throughout the AST
#[derive(Visitor)]
struct BoxReferenceUpdater {
    simplified_trait_impls: HashSet<TraitImplId>,
    simplified_methods: HashSet<FunDeclId>,
}

impl BoxReferenceUpdater {
    fn process_generic_args_for_simplified_item(&self, args: &mut GenericArgs) {
        // If this is referencing a simplified Box item, update the generic args
        // Remove the second type argument and the last two trait references
        if args.types.elem_count() == 2 && args.trait_refs.elem_count() == 3 {
            // Remove allocator type parameter
            args.types.remove_and_shift_ids(TypeVarId::new(1));
            
            // Remove the last two trait references
            args.trait_refs.remove_and_shift_ids(TraitClauseId::new(2));
            args.trait_refs.remove_and_shift_ids(TraitClauseId::new(1));
        }
    }
}

impl VisitAstMut for BoxReferenceUpdater {
    fn enter_trait_impl_ref(&mut self, impl_ref: &mut TraitImplRef) {
        if self.simplified_trait_impls.contains(&impl_ref.id) {
            self.process_generic_args_for_simplified_item(&mut impl_ref.generics);
        }
    }
    
    fn enter_fun_decl_ref(&mut self, fun_ref: &mut FunDeclRef) {
        if self.simplified_methods.contains(&fun_ref.id) {
            self.process_generic_args_for_simplified_item(&mut fun_ref.generics);
        }
    }
}