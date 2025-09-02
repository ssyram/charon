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

use crate::{ast::*, transform::TransformCtx};

use super::ctx::TransformPass;

pub struct Transform;

impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        // Force output to stdout and stderr
        eprintln!("STDERR: SIMPLIFY BOX TRANSFORM WAS CALLED!");
        println!("STDOUT: SIMPLIFY BOX TRANSFORM WAS CALLED!");
        
        // Try to create a file to verify the transform runs
        std::fs::write("/tmp/transform_ran.txt", "Transform was called").unwrap();
        
        if ctx.options.raw_boxes {
            eprintln!("Raw boxes enabled, skipping simplification");
            return;
        }

        // Find the unique impl Drop for Box trait implementation
        let mut box_trait_impl_id = None;
        for timpl in ctx.translated.trait_impls.iter() {
            if is_box_drop_impl(timpl, ctx) {
                println!("Found Box Drop trait impl: {:?}", timpl.item_meta.name);
                box_trait_impl_id = Some(timpl.def_id);
                break; // Should be unique
            }
        }
        
        // Find the unique drop function for Box
        let mut box_drop_fun_id = None;
        for fun_decl in ctx.translated.fun_decls.iter() {
            if is_box_drop_method(fun_decl, ctx) {
                println!("Found Box Drop method: {:?}", fun_decl.item_meta.name);
                box_drop_fun_id = Some(fun_decl.def_id);
                break; // Should be unique
            }
        }

        println!("Box trait impl ID: {:?}, Box drop fun ID: {:?}", box_trait_impl_id, box_drop_fun_id);

        // Apply simplifications
        if let Some(impl_id) = box_trait_impl_id {
            if let Some(timpl) = ctx.translated.trait_impls.get_mut(impl_id) {
                println!("Simplifying trait impl: {:?}", timpl.item_meta.name);
                
                // 1. Change the trait impl's generic parameters
                simplify_generic_params(&mut timpl.generics);
                
                // 2. Change its reference to its internal drop method in methods list
                for (_, method_ref) in &mut timpl.methods {
                    simplify_method_ref_generics(&mut method_ref.skip_binder);
                }
            }
        }
        
        if let Some(fun_id) = box_drop_fun_id {
            if let Some(fun_decl) = ctx.translated.fun_decls.get_mut(fun_id) {
                println!("Simplifying method: {:?}", fun_decl.item_meta.name);
                
                // Change the function's generic parameters
                simplify_generic_params(&mut fun_decl.signature.generics);
            }
        }

        println!("Box simplification transform completed");
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
    if let ItemKind::TraitImpl {
        trait_ref,
        item_name,
        ..
    } = &fun_decl.kind
    {
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

/// Simplify GenericParams for Box Drop implementations by removing allocator type parameter and all trait clauses
fn simplify_generic_params(generics: &mut GenericParams) {
    trace!("Simplifying generic params - before: {} type params, {} trait clauses", 
           generics.types.elem_count(), generics.trait_clauses.elem_count());
    
    // Remove allocator type parameter (A) - keep only T
    if generics.types.elem_count() >= 2 {
        generics.types.remove(TypeVarId::new(1));
    }
    
    // Remove ALL trait clauses (MetaSized, Sized, Allocator)
    let clause_ids: Vec<TraitClauseId> = generics.trait_clauses.all_indices().collect();
    for clause_id in clause_ids {
        generics.trait_clauses.remove(clause_id);
    }
    
    trace!("Simplifying generic params - after: {} type params, {} trait clauses", 
           generics.types.elem_count(), generics.trait_clauses.elem_count());
}

/// Simplify method reference generics for Box Drop implementations
fn simplify_method_ref_generics(method_ref: &mut FunDeclRef) {
    trace!("Simplifying method ref generics");
    
    // Remove allocator type parameter (A) - keep only T  
    if method_ref.generics.types.elem_count() >= 2 {
        method_ref.generics.types.remove(TypeVarId::new(1));
    }
    
    // Remove ALL trait references
    let trait_ref_ids: Vec<TraitClauseId> = method_ref.generics.trait_refs.all_indices().collect();
    for trait_ref_id in trait_ref_ids {
        method_ref.generics.trait_refs.remove(trait_ref_id);
    }
}

/// Check if a type is a Box type (either Builtin::Box or a type-decl with Box lang-item)
fn is_box_type(ty: &Ty, ctx: &TransformCtx) -> bool {
    match ty.kind() {
        TyKind::Adt(type_decl_ref) => match &type_decl_ref.id {
            TypeId::Builtin(BuiltinTy::Box) => true,
            TypeId::Adt(type_decl_id) => {
                if let Some(type_decl) = ctx.translated.type_decls.get(*type_decl_id) {
                    type_decl.item_meta.lang_item.as_deref() == Some("owned_box")
                } else {
                    false
                }
            }
            _ => false,
        },
        _ => false,
    }
}
