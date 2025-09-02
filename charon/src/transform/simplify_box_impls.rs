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

use super::ctx::TransformPass;

pub struct Transform;

impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        trace!("=== SIMPLIFY BOX IMPLS TRANSFORM STARTED ===");
        
        if ctx.options.raw_boxes {
            trace!("Raw boxes enabled, skipping simplification");
            return;
        }

        trace!("Total trait impls: {}", ctx.translated.trait_impls.elem_count());
        trace!("Total fun decls: {}", ctx.translated.fun_decls.elem_count());
        
        // First collect the IDs that need to be simplified to avoid borrowing conflicts
        let mut box_trait_impls_to_simplify = Vec::new();
        let mut box_methods_to_simplify = Vec::new();
        
        // Traverse all trait implementations to find Box Drop implementations
        for timpl in ctx.translated.trait_impls.iter() {
            if is_box_drop_impl(timpl, ctx) {
                trace!("Found Box Drop trait impl: {:?}", timpl.item_meta.name);
                box_trait_impls_to_simplify.push(timpl.def_id);
            }
        }
        
        // Traverse all function declarations to find Box Drop methods
        for fun_decl in ctx.translated.fun_decls.iter() {
            if is_box_drop_method(fun_decl, ctx) {
                trace!("Found Box Drop method: {:?}", fun_decl.item_meta.name);
                box_methods_to_simplify.push(fun_decl.def_id);
            }
        }

        trace!("Box implementations to simplify: {} trait impls, {} methods", 
               box_trait_impls_to_simplify.len(), box_methods_to_simplify.len());

        // Apply simplification to trait implementations
        for impl_id in box_trait_impls_to_simplify {
            if let Some(timpl) = ctx.translated.trait_impls.get_mut(impl_id) {
                trace!("Simplifying trait impl: {:?}", timpl.item_meta.name);
                simplify_box_generic_params(&mut timpl.generics);
                simplify_box_trait_decl_ref(&mut timpl.impl_trait);
            }
        }
        
        // Apply simplification to methods
        for method_id in box_methods_to_simplify {
            if let Some(fun_decl) = ctx.translated.fun_decls.get_mut(method_id) {
                trace!("Simplifying method: {:?}", fun_decl.item_meta.name);
                simplify_box_generic_params(&mut fun_decl.signature.generics);
                
                // Also update trait ref in function kind if it's a trait impl method
                if let ItemKind::TraitImpl { trait_ref, .. } = &mut fun_decl.kind {
                    simplify_box_trait_decl_ref(trait_ref);
                }
            }
        }
    }
}

/// Check if this trait implementation is a Box Drop implementation
fn is_box_drop_impl(timpl: &TraitImpl, ctx: &TransformCtx) -> bool {
    // Check if the trait-ref's id is for "drop" by checking the lang-item
    if let Some(trait_decl) = ctx.translated.trait_decls.get(timpl.impl_trait.id) {
        if trait_decl.item_meta.lang_item.as_deref() != Some("drop") {
            return false;
        }
    } else {
        return false;
    }

    // Check if the first generic type's type-id is Builtin::Box or a type-decl-ID with the Box lang-item
    if let Some(first_arg) = timpl.impl_trait.generics.types.get(TypeVarId::new(0)) {
        return is_box_type(first_arg, ctx);
    }

    false
}

/// Check if this function is a Box Drop method
fn is_box_drop_method(fun_decl: &FunDecl, ctx: &TransformCtx) -> bool {
    if let ItemKind::TraitImpl { trait_ref, item_name, .. } = &fun_decl.kind {
        // Check if this is a Drop trait and if the item is drop
        if item_name.0 == "drop" {
            if let Some(trait_decl) = ctx.translated.trait_decls.get(trait_ref.id) {
                if trait_decl.item_meta.lang_item.as_deref() == Some("drop") {
                    // Check if the first generic type is Box
                    if let Some(first_arg) = trait_ref.generics.types.get(TypeVarId::new(0)) {
                        return is_box_type(first_arg, ctx);
                    }
                }
            }
        }
    }
    false
}

/// Simplify GenericParams for Box Drop implementations
/// Removes allocator type parameter and all trait clauses
fn simplify_box_generic_params(generics: &mut GenericParams) {
    trace!("Simplifying generic params - before: {} type params, {} trait clauses", 
           generics.types.elem_count(), generics.trait_clauses.elem_count());
    
    // Remove allocator type parameter (keep only T, remove A)
    // Box Drop impl has pattern: impl<T, A> Drop for Box<T, A>
    // We want: impl<T> Drop for Box<T>
    if generics.types.elem_count() >= 2 {
        // Remove the allocator parameter (second type param)
        generics.types.remove(TypeVarId::new(1));
    }
    
    // Remove ALL trait clauses
    let clause_ids: Vec<TraitClauseId> = generics.trait_clauses.all_indices().collect();
    for clause_id in clause_ids {
        generics.trait_clauses.remove(clause_id);
    }
    
    trace!("Simplifying generic params - after: {} type params, {} trait clauses", 
           generics.types.elem_count(), generics.trait_clauses.elem_count());
}

/// Simplify TraitDeclRef for Box Drop implementations  
/// Changes alloc::boxed::Box<T, A> to builtin Box<T>
fn simplify_box_trait_decl_ref(trait_decl_ref: &mut TraitDeclRef) {
    trace!("Simplifying trait decl ref");
    
    // Update the first type argument from alloc::boxed::Box<T, A> to Box<T>
    if let Some(first_type) = trait_decl_ref.generics.types.get_mut(TypeVarId::new(0)) {
        // We need to modify the Ty in place
        if let TyKind::Adt(type_decl_ref) = first_type.kind() {
            if matches!(type_decl_ref.id, TypeId::Adt(_)) {
                // Create a new Ty with builtin Box
                let new_ty_kind = TyKind::Adt(TypeDeclRef {
                    id: TypeId::Builtin(BuiltinTy::Box),
                    generics: {
                        let mut new_generics = type_decl_ref.generics.clone();
                        // Remove allocator type argument (keep only T)
                        if new_generics.types.elem_count() >= 2 {
                            new_generics.types.remove(TypeVarId::new(1));
                        }
                        // Remove all trait clause references
                        let clause_ids: Vec<TraitClauseId> = new_generics.trait_refs.all_indices().collect();
                        for clause_id in clause_ids {
                            new_generics.trait_refs.remove(clause_id);
                        }
                        new_generics
                    },
                });
                *first_type = Ty::new(new_ty_kind);
            }
        }
    }
}

/// Check if a type is a Box type (either Builtin::Box or a type-decl with Box lang-item)
fn is_box_type(ty: &Ty, ctx: &TransformCtx) -> bool {
    match ty.kind() {
        TyKind::Adt(type_decl_ref) => {
            match &type_decl_ref.id {
                TypeId::Builtin(BuiltinTy::Box) => true,
                TypeId::Adt(type_decl_id) => {
                    if let Some(type_decl) = ctx.translated.type_decls.get(*type_decl_id) {
                        type_decl.item_meta.lang_item.as_deref() == Some("owned_box")
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }
        _ => false,
    }
}