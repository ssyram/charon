//! Simplify Box implementations when not in raw-boxes mode.
//!
//! When `--raw-boxes` is not enabled, we want builtin `Box<T>` with no trait constraints
//! instead of the complex `alloc::boxed::Box<T>` with allocator parameters and trait clauses.
//!
//! This pass transforms (after hide_allocator_param has already run):
//! - `impl<T, A> Drop for alloc::boxed::Box<T>[@TraitClause0, @TraitClause1] where MetaSized<T>, Sized<A>`
//! 
//! To:
//! - `impl<T> Drop for Box<T>` (builtin Box, no trait clauses)

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
        // Temporarily disable all conditions to test if transform runs
        if ctx.options.raw_boxes {
            // Don't simplify when raw-boxes is enabled
            return;
        }
        
        // if !ctx.options.hide_allocator {
        //     // Only simplify when hide_allocator is enabled (similar to hide_allocator_param)
        //     return;
        // }

        // First, identify trait implementations that need to be simplified
        let mut box_trait_impls_to_replace = Vec::new();
        let mut box_methods_to_replace = Vec::new();
        
        // COMPILATION TEST - this should cause an error if the code is being compiled
        // let _intentional_error = undefined_variable;
        
        // Simplified test: just try to transform ANY trait implementation with trait clauses
        for timpl in ctx.translated.trait_impls.iter() {
            if timpl.generics.trait_clauses.elem_count() > 0 {
                box_trait_impls_to_replace.push(timpl.def_id);
            }
        }
        
        for fun_decl in ctx.translated.fun_decls.iter() {
            if is_box_allocator_drop_method(&fun_decl, ctx) {
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

/// Check if this trait implementation is a Box Drop implementation
fn is_box_allocator_drop_impl(timpl: &TraitImpl, _ctx: &TransformCtx) -> bool {
    // After hide_allocator_param, Box Drop implementations will have trait clauses we want to remove
    // Be more general: any trait implementation with trait clauses that mentions Box in the name
    let name_str = format!("{:?}", timpl.item_meta.name);
    
    // Look for implementations that mention Box and Drop
    let has_box_drop_name = name_str.contains("Box") && name_str.contains("Drop");
    
    // Also check if this has trait clauses that we want to remove
    let has_trait_clauses = timpl.generics.trait_clauses.elem_count() > 0;
    
    has_box_drop_name && has_trait_clauses
}

/// Check if this function is a Box Drop method
fn is_box_allocator_drop_method(fun_decl: &FunDecl, _ctx: &TransformCtx) -> bool {
    if let ItemKind::TraitImpl { item_name, .. } = &fun_decl.kind {
        if item_name.0 == "drop" {
            // Check if this is a Box-related Drop method by name
            let name_str = format!("{:?}", fun_decl.item_meta.name);
            return name_str.contains("alloc") && 
                   name_str.contains("boxed") && 
                   name_str.contains("Box") && 
                   name_str.contains("Drop");
        }
    }
    false
}

/// Create a simplified Box Drop trait implementation from a complex one
fn create_simplified_box_drop_impl(original: &TraitImpl) -> TraitImpl {
    // Create simplified generics with only the first type parameter (T)
    let mut simplified_generics = original.generics.clone();
    
    // Keep only the first type parameter (T), remove allocator (A)
    if simplified_generics.types.elem_count() >= 2 {
        // Remove the second type parameter (allocator)
        simplified_generics.types.remove_and_shift_ids(TypeVarId::new(1));
    }
    
    // Remove ALL trait clauses as requested - we don't need MetaSized<T> either
    while simplified_generics.trait_clauses.elem_count() > 0 {
        simplified_generics.trait_clauses.remove_and_shift_ids(TraitClauseId::new(0));
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
    
    // Keep only the first type parameter (T), remove allocator (A)
    if simplified_generics.types.elem_count() >= 2 {
        // Remove the second type parameter (allocator)
        simplified_generics.types.remove_and_shift_ids(TypeVarId::new(1));
    }
    
    // Remove ALL trait clauses as requested
    while simplified_generics.trait_clauses.elem_count() > 0 {
        simplified_generics.trait_clauses.remove_and_shift_ids(TraitClauseId::new(0));
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

/// Visitor to transform alloc::boxed::Box<T> references to builtin Box<T> with no trait clauses
#[derive(Visitor)]
struct BoxTypeSimplifier;

impl VisitAstMut for BoxTypeSimplifier {
    fn enter_type_decl_ref(&mut self, ty_ref: &mut TypeDeclRef) {
        // Only transform ADT types that could be alloc::boxed::Box
        match &ty_ref.id {
            TypeId::Adt(_) => {
                // This might be alloc::boxed::Box<T> that should be converted to builtin Box<T>
                // Be conservative: only convert if it has exactly 1 type parameter and some trait refs
                if ty_ref.generics.types.elem_count() == 1 && ty_ref.generics.trait_refs.elem_count() >= 1 {
                    // Change to builtin Box type
                    ty_ref.id = TypeId::Builtin(BuiltinTy::Box);
                    
                    // Remove ALL trait references as requested
                    while ty_ref.generics.trait_refs.elem_count() > 0 {
                        ty_ref.generics.trait_refs.remove_and_shift_ids(TraitClauseId::new(0));
                    }
                }
            }
            TypeId::Builtin(BuiltinTy::Box) => {
                // Already a builtin Box, just remove any remaining trait refs
                while ty_ref.generics.trait_refs.elem_count() > 0 {
                    ty_ref.generics.trait_refs.remove_and_shift_ids(TraitClauseId::new(0));
                }
            }
            _ => {
                // Not a Box type, don't modify
            }
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
        // Remove ALL trait references as requested
        while args.trait_refs.elem_count() > 0 {
            args.trait_refs.remove_and_shift_ids(TraitClauseId::new(0));
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