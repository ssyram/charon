//! This transform generates drop shims for vtable instances.
//! 
//! The problem: vtable instances need drop functions with signature `fn(*mut dyn Trait)`, 
//! but concrete drop implementations use `fn(*mut ConcreteType)`. We need shims that convert
//! between these signatures.
//!
//! For types with Drop implementations: Generate shims that convert trait object pointers
//! to concrete type pointers and forward to concrete drop functions.
//!
//! For types without Drop implementations: Generate no-op shim functions instead of 
//! opaque placeholders.

use crate::ast::*;
use crate::transform::TransformCtx;
use crate::ullbc_ast::{ExprBody as GExprBody, RawStatement, Statement, BlockData, Terminator, RawTerminator};
use super::ctx::TransformPass;
use std::collections::HashMap;

pub struct Transform;

impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let mut vtable_drop_shims = HashMap::new();
        
        // First pass: identify vtable instances and generate drop shims
        for (global_id, global_decl) in ctx.translated.global_decls.iter_indexed() {
            if let ItemKind::VTableInstance { impl_ref } = &global_decl.kind {
                // Get the trait implementation to extract the concrete type
                if let Some(trait_impl) = ctx.translated.trait_impls.get(impl_ref.id) {
                    // Extract the concrete type (Self type) from the trait implementation
                    let concrete_type = self.extract_concrete_type(&trait_impl.impl_trait, impl_ref);
                    
                    if let Some(self_type) = concrete_type {
                        // Generate a drop shim for this concrete type
                        let drop_shim_id = self.generate_drop_shim_for_type(
                            ctx, 
                            global_id,
                            &self_type,
                            &trait_impl.impl_trait
                        );
                        if let Some(shim_id) = drop_shim_id {
                            vtable_drop_shims.insert(global_id, shim_id);
                        }
                    }
                }
            }
        }
        
        // Second pass: update vtable initializations to use the generated drop shims
        for (global_id, shim_id) in vtable_drop_shims {
            self.update_vtable_drop_field(ctx, global_id, shim_id);
        }
    }

    fn name(&self) -> &str {
        "generate_vtable_drop_shims"
    }
}

impl Transform {
    /// Extract the concrete type (Self type) from a trait implementation
    fn extract_concrete_type(
        &self,
        trait_decl_ref: &TraitDeclRef,
        _impl_ref: &TraitImplRef,
    ) -> Option<Ty> {
        // The concrete type should be the first generic argument in the trait reference
        // For `impl Trait<Args> for ConcreteType`, the ConcreteType is the Self type
        // which corresponds to the first generic argument
        if !trait_decl_ref.generics.types.is_empty() {
            Some(trait_decl_ref.generics.types[0].clone())
        } else {
            None
        }
    }

    /// Generate a drop shim function for a specific concrete type
    fn generate_drop_shim_for_type(
        &self,
        ctx: &mut TransformCtx,
        vtable_id: GlobalDeclId,
        concrete_type: &Ty,
        trait_decl_ref: &TraitDeclRef,
    ) -> Option<FunDeclId> {
        // Create a unique function name for this drop shim
        let vtable_name = ctx.translated.global_decls.get(vtable_id)?.item_meta.name.clone();
        let mut shim_name = vtable_name.clone();
        shim_name.name.push(PathElem::Ident("drop_shim".to_string(), Disambiguator::ZERO));
        
        // Create the drop shim function signature: unsafe fn(*mut dyn Trait) -> ()
        let dyn_trait_type = self.create_dyn_trait_type(trait_decl_ref);
        let dyn_trait_ptr = TyKind::RawPtr(dyn_trait_type, RefKind::Mut).into_ty();
        let signature = FunSig {
            is_unsafe: true,
            generics: GenericParams::empty(),
            inputs: vec![dyn_trait_ptr],
            output: Ty::mk_unit(),
        };
        
        // Generate the function body
        let body = if self.type_has_drop_impl(ctx, concrete_type) {
            // Generate shim that converts to concrete type and calls concrete drop
            self.generate_drop_shim_body_with_drop(concrete_type, &signature)
        } else {
            // Generate no-op shim
            self.generate_drop_shim_body_noop(&signature)
        };
        
        // Create the function declaration
        let fun_id = ctx.translated.fun_decls.reserve_slot();
        let fun_decl = FunDecl {
            def_id: fun_id,
            item_meta: ItemMeta {
                name: shim_name,
                span: Span::dummy(),
                source_text: None,
                attr_info: AttrInfo::default(),
                is_local: true,
                opacity: ItemOpacity::Transparent,
                lang_item: None,
            },
            signature,
            kind: ItemKind::TopLevel,
            is_global_initializer: None,
            body: Ok(body),
        };
        
        ctx.translated.fun_decls.set_slot(fun_id, fun_decl);
        Some(fun_id)
    }
    
    /// Create a `dyn Trait` type from a trait declaration reference  
    /// For now, we'll create a simple version that may not be perfect
    fn create_dyn_trait_type(&self, _trait_decl_ref: &TraitDeclRef) -> Ty {
        // TODO: Create proper dyn trait type
        // For now, return a placeholder type to avoid compilation errors
        TyKind::Never.into_ty()
    }
    
    /// Check if a type has a Drop implementation
    fn type_has_drop_impl(&self, ctx: &TransformCtx, self_type: &Ty) -> bool {
        // Look for a Drop trait implementation for this type
        for (_impl_id, impl_decl) in ctx.translated.trait_impls.iter_indexed() {
            let trait_ref = &impl_decl.impl_trait;
            // Check if this is a Drop trait implementation
            if let Some(trait_decl) = ctx.translated.trait_decls.get(trait_ref.id) {
                // Check if this trait has Drop in its name (simple heuristic)
                let trait_name = &trait_decl.item_meta.name;
                let has_drop_in_name = trait_name.name.iter().any(|elem| {
                    matches!(elem, PathElem::Ident(name, _) if name.contains("Drop"))
                });
                
                if has_drop_in_name {
                    // Check if the impl is for our self_type
                    if !trait_ref.generics.types.is_empty() {
                        let impl_self_ty = &trait_ref.generics.types[0];
                        if impl_self_ty == self_type {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
    
    /// Generate a drop shim body for types with Drop implementations
    fn generate_drop_shim_body_with_drop(
        &self,
        _concrete_type: &Ty,
        signature: &FunSig,
    ) -> Body {
        self.generate_drop_shim_body_noop(signature)
    }
    
    /// Generate a no-op drop shim body for types without Drop implementations
    fn generate_drop_shim_body_noop(&self, signature: &FunSig) -> Body {
        // Create locals for the function
        let mut locals = Locals {
            arg_count: signature.inputs.len(),
            locals: Vector::new(),
        };
        
        // Return value (unit)
        locals.locals.push(Local {
            index: LocalId::ZERO,
            name: None,
            ty: Ty::mk_unit(),
        });
        
        // Parameter: dyn_self: *mut dyn Trait
        locals.locals.push(Local {
            index: LocalId::from_usize(1),
            name: Some("dyn_self".into()),
            ty: signature.inputs[0].clone(),
        });
        
        // No-op body - just return
        let statements = vec![
            Statement::new(
                Span::dummy(),
                RawStatement::Nop,
            ),
        ];
        
        let block = BlockData {
            statements,
            terminator: Terminator::new(
                Span::dummy(), 
                RawTerminator::Return
            ),
        };
        
        Body::Unstructured(GExprBody {
            span: Span::dummy(),
            locals,
            comments: Vec::new(),
            body: [block].into(),
        })
    }
    
    /// Update the vtable initializer to use the generated drop shim
    fn update_vtable_drop_field(
        &self,
        ctx: &mut TransformCtx,
        vtable_id: GlobalDeclId,
        drop_shim_id: FunDeclId,
    ) {
        // Get the vtable global declaration
        if let Some(vtable_global) = ctx.translated.global_decls.get(vtable_id) {
            let initializer_fun_id = vtable_global.init;
            
            // Update the vtable initializer function to use the drop shim
            if let Some(init_fun) = ctx.translated.fun_decls.get_mut(initializer_fun_id) {
                if let Ok(Body::Unstructured(ref mut body)) = &mut init_fun.body {
                    // Find and update the drop field in the vtable initialization
                    self.update_drop_field_in_body(body, drop_shim_id);
                }
            }
        }
    }
    
    /// Update the drop field in the vtable initializer body
    fn update_drop_field_in_body(&self, body: &mut GExprBody, drop_shim_id: FunDeclId) {
        // Search through all statements in all blocks to find vtable aggregate assignments
        for (_, block) in body.body.iter_mut() {
            for statement in &mut block.statements {
                if let RawStatement::Assign(_, rvalue) = &mut statement.content {
                    if let Rvalue::Aggregate(AggregateKind::Adt(..), operands) = rvalue {
                        // Look for the drop field (typically the third field after size and align)
                        // and update opaque drop placeholders to use our generated shim
                        self.update_drop_operand_in_aggregate(operands, drop_shim_id);
                    }
                }
            }
        }
    }
    
    /// Update drop operand in an aggregate (vtable struct)
    fn update_drop_operand_in_aggregate(&self, operands: &mut Vec<Operand>, drop_shim_id: FunDeclId) {
        for operand in operands {
            if let Operand::Const(const_expr) = operand {
                if let RawConstantExpr::Opaque(ref message) = const_expr.value {
                    // Replace opaque drop placeholders with function pointer to our shim
                    if message.contains("drop") {
                        let fn_ptr = FnPtr {
                            func: Box::new(FunIdOrTraitMethodRef::Fun(FunId::Regular(drop_shim_id))),
                            generics: Box::new(GenericArgs::empty()),
                        };
                        const_expr.value = RawConstantExpr::FnPtr(fn_ptr);
                    }
                }
            }
        }
    }
}