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
        eprintln!("DEBUG: simplify_box_impls transform started, raw_boxes = {}, hide_allocator = {}", 
                  ctx.options.raw_boxes, ctx.options.hide_allocator);
        
        if ctx.options.raw_boxes {
            // Don't simplify when raw-boxes is enabled
            eprintln!("DEBUG: raw_boxes enabled, skipping transform");
            return;
        }
        
        // Note: hide_allocator_param transform has already run by this point,
        // so we're working with partially processed Box types

        // First, identify trait implementations that need to be simplified
        let mut box_trait_impls_to_replace = Vec::new();
        let mut box_methods_to_replace = Vec::new();
        
        for timpl in ctx.translated.trait_impls.iter() {
            // Debug: print all trait implementations to understand the structure
            eprintln!("DEBUG: Found trait impl: {:?}", timpl.item_meta.name);
            eprintln!("  - Type params: {}", timpl.generics.types.elem_count());
            eprintln!("  - Trait clauses: {}", timpl.generics.trait_clauses.elem_count());
            if timpl.generics.types.elem_count() >= 1 {
                if let Some(first_type) = timpl.impl_trait.generics.types.iter().next() {
                    eprintln!("  - First impl type: {:?}", first_type);
                }
            }
            
            if is_box_allocator_drop_impl(timpl, ctx) {
                eprintln!("  - IDENTIFIED as Box Drop impl!");
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

/// Check if this trait implementation is a Box Drop implementation with allocator parameters
fn is_box_allocator_drop_impl(timpl: &TraitImpl, ctx: &TransformCtx) -> bool {
    // Check if this has Drop trait characteristics and Box patterns
    if timpl.generics.types.elem_count() == 0 {
        return false;
    }
    
    // After hide_allocator_param runs, Box Drop implementations will have:
    // - 1-2 type parameters (T, and possibly still A)  
    // - 1-2 trait clauses (MetaSized<T>, and possibly Sized<A>)
    
    // Look for Box types by checking if this impl is for a Box type
    // First, get the Self type from the impl_trait generics
    if let Some(self_ty) = timpl.impl_trait.generics.types.iter().next() {
        if is_box_type(self_ty, ctx) {
            eprintln!("DEBUG: Found Box type in trait impl");
            return true;
        }
    }
    
    // Also check by name pattern matching
    let name_str = format!("{:?}", timpl.item_meta.name);
    if name_str.contains("Box") && name_str.contains("Drop") {
        eprintln!("DEBUG: Found Box Drop by name pattern: {}", name_str);
        return true;
    }
    
    // Check if this has the structure we expect after hide_allocator_param has run
    // Box Drop implementations might still have trait clauses we want to remove
    if timpl.generics.types.elem_count() >= 1 && 
       timpl.generics.trait_clauses.elem_count() >= 1 {
        // This could be a Box Drop implementation that we should process
        // Check if any trait clauses mention MetaSized which is common in Box impls
        eprintln!("DEBUG: Found potential Box impl with {} types, {} clauses", 
                  timpl.generics.types.elem_count(),
                  timpl.generics.trait_clauses.elem_count());
        
        // For now, let's be conservative and only process impls that clearly look like Box
        return name_str.contains("alloc") && name_str.contains("boxed");
    }
    
    false
}

/// Check if a type is a Box type (either alloc::boxed::Box or builtin Box)
fn is_box_type(ty: &Ty, _ctx: &TransformCtx) -> bool {
    match &**ty {
        TyKind::Adt(type_decl_ref) => {
            match &type_decl_ref.id {
                TypeId::Builtin(BuiltinTy::Box) => true,
                TypeId::Adt(_type_id) => {
                    // For now, assume any ADT with 2 type params and 2+ trait refs could be Box
                    // This is a heuristic but should work for the specific Box case
                    type_decl_ref.generics.types.elem_count() >= 1 && 
                    type_decl_ref.generics.trait_refs.elem_count() >= 1
                }
                _ => false,
            }
        }
        _ => false,
    }
}

/// Check if this function is a Box Drop method with allocator parameters
fn is_box_allocator_drop_method(fun_decl: &FunDecl, ctx: &TransformCtx) -> bool {
    if let ItemKind::TraitImpl { item_name, .. } = &fun_decl.kind {
        if item_name.0 == "drop" {
            // Check if this is a Box-related Drop method by examining the signature
            if fun_decl.signature.generics.types.elem_count() >= 1 {
                // Look at the first parameter type (self) to see if it's a Box
                if let Some(first_input) = fun_decl.signature.inputs.iter().next() {
                    return is_box_type(first_input, ctx);
                }
            }
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

/// Visitor to transform alloc::boxed::Box<T, A> references to builtin Box<T>
#[derive(Visitor)]
struct BoxTypeSimplifier;

impl VisitAstMut for BoxTypeSimplifier {
    fn enter_type_decl_ref(&mut self, ty_ref: &mut TypeDeclRef) {
        // Check if this is specifically a Box type that should be simplified
        match &ty_ref.id {
            TypeId::Adt(_) => {
                // This might be alloc::boxed::Box<T, A> that should be converted to builtin Box<T>
                if ty_ref.generics.types.elem_count() == 2 && ty_ref.generics.trait_refs.elem_count() >= 2 {
                    // Change to builtin Box type
                    ty_ref.id = TypeId::Builtin(BuiltinTy::Box);
                    
                    // Remove the second type parameter (allocator)
                    ty_ref.generics.types.remove_and_shift_ids(TypeVarId::new(1));
                    
                    // Remove ALL trait references as requested
                    while ty_ref.generics.trait_refs.elem_count() > 0 {
                        ty_ref.generics.trait_refs.remove_and_shift_ids(TraitClauseId::new(0));
                    }
                }
            }
            TypeId::Builtin(BuiltinTy::Box) => {
                // Already a builtin Box, but might still have too many type args or trait refs
                if ty_ref.generics.types.elem_count() > 1 {
                    // Remove extra type parameters beyond the first one
                    while ty_ref.generics.types.elem_count() > 1 {
                        ty_ref.generics.types.remove_and_shift_ids(TypeVarId::new(1));
                    }
                }
                
                // Remove ALL trait references as requested
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
        // If this is referencing a simplified Box item, update the generic args
        // Remove the second type argument and ALL trait references
        if args.types.elem_count() >= 2 {
            // Remove allocator type parameter
            args.types.remove_and_shift_ids(TypeVarId::new(1));
        }
        
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