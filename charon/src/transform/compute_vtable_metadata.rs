//! Compute metadata for vtable instances (size, align, drop).
//!
//! This pass fills the metadata fields of vtable instances with correct values instead of opaque placeholders.
//!
//! For each vtable instance initializer function, we:
//! 1. Extract the concrete type being implemented for
//! 2. Compute size & align from the type's layout information
//! 3. Generate proper drop shim functions for the drop field
//! 4. Replace the opaque placeholders with the actual values

use super::ctx::TransformPass;
use crate::{
    ast::ScalarValue, errors::Error, formatter::IntoFormatter, pretty::FmtWithCtx, raise_error,
    register_error, transform::TransformCtx, ullbc_ast::*,
};

pub struct Transform;

impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        trace!(
            "ComputeVtableMetadata: Processing {} vtable instances",
            count_vtable_instances(ctx)
        );

        // Process vtable instance initializer functions
        ctx.for_each_fun_decl(|ctx, decl| {
            if let ItemKind::VTableInstance { impl_ref } = &decl.kind {
                if let Ok(body) = &mut decl.body {
                    let mut computer =
                        VtableMetadataComputer::new(ctx, impl_ref, decl.item_meta.span);

                    match computer.compute_vtable_metadata_for_function(body) {
                        Ok(_) => {
                            trace!(
                                "Successfully computed vtable metadata for {}",
                                decl.def_id.with_ctx(&ctx.into_fmt())
                            );
                        }
                        Err(e) => {
                            register_error!(
                                ctx,
                                decl.item_meta.span,
                                "Failed to compute vtable metadata: {:?}",
                                e
                            );
                        }
                    }
                }
            }
        });
    }

    fn name(&self) -> &str {
        "ComputeVtableMetadata"
    }
}

/// Vtable metadata computer that holds common state and provides methods
/// for computing size, align, and drop shim functions for vtable instances.
struct VtableMetadataComputer<'a> {
    ctx: &'a mut TransformCtx,
    impl_ref: &'a TraitImplRef,
    span: Span,
}

impl<'a> VtableMetadataComputer<'a> {
    fn new(ctx: &'a mut TransformCtx, impl_ref: &'a TraitImplRef, span: Span) -> Self {
        Self {
            ctx,
            impl_ref,
            span,
        }
    }

    /// Compute vtable metadata for a specific vtable instance initializer function
    fn compute_vtable_metadata_for_function(&mut self, body: &mut Body) -> Result<(), Error> {
        let Body::Unstructured(expr_body) = body else {
            // Skip structured bodies as they should not contain vtable instances
            return Ok(());
        };

        // Find the vtable initialization statement
        for block in expr_body.body.iter_mut() {
            for stmt in &mut block.statements {
                if let RawStatement::Assign(_place, rvalue) = &mut stmt.content {
                    if let Rvalue::Aggregate(AggregateKind::Adt(vtable_ref, None, None), fields) =
                        rvalue
                    {
                        if self.is_vtable_struct(&vtable_ref.id)? {
                            self.update_vtable_metadata(vtable_ref, fields)?;
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Check if a type ID represents a vtable struct by getting the correct vtable def-id
    /// from the impl-ref: impl-ref → def-id of implemented trait → get definition of the trait → get the vtable-ref → get the type-def-id of target vtable
    fn is_vtable_struct(&self, type_id: &TypeId) -> Result<bool, Error> {
        let TypeId::Adt(type_decl_id) = type_id else {
            return Ok(false);
        };

        // Get the trait implementation
        let Some(trait_impl) = self.ctx.translated.trait_impls.get(self.impl_ref.id) else {
            raise_error!(
                self.ctx,
                self.span,
                "Trait impl not found: {}",
                self.impl_ref.id.with_ctx(&self.ctx.into_fmt())
            );
        };

        // Get the implemented trait
        let trait_decl_ref = &trait_impl.impl_trait;
        let Some(trait_decl) = self.ctx.translated.trait_decls.get(trait_decl_ref.id) else {
            raise_error!(
                self.ctx,
                self.span,
                "Trait declaration not found: {}",
                trait_decl_ref.id.with_ctx(&self.ctx.into_fmt())
            );
        };

        // Get the vtable ref from the trait definition
        let Some(vtable_ref) = &trait_decl.vtable else {
            return Ok(false); // Trait has no vtable (not dyn-compatible)
        };

        // Check if the type ID matches the vtable's type ID
        Ok(vtable_ref.id == TypeId::Adt(*type_decl_id))
    }

    /// Update the vtable metadata fields (size, align, drop) with correct values
    fn update_vtable_metadata(
        &mut self,
        _vtable_ref: &TypeDeclRef,
        fields: &mut Vec<Operand>,
    ) -> Result<(), Error> {
        // We expect fields in order: size, align, drop, method1, method2, ..., supertrait1, ...
        if fields.len() < 3 {
            raise_error!(
                self.ctx,
                self.span,
                "Expected at least 3 fields in vtable (size, align, drop)"
            );
        }

        // Get the concrete type from the impl
        let concrete_ty = self.get_concrete_type_from_impl()?;

        // 1. Update size field
        fields[0] = self.compute_size_operand(&concrete_ty)?;

        // 2. Update align field
        fields[1] = self.compute_align_operand(&concrete_ty)?;

        // 3. Update drop field - generate actual shim function instead of opaque
        fields[2] = self.generate_drop_shim(&concrete_ty)?;

        Ok(())
    }

    /// Extract the concrete type being implemented for from the trait impl reference
    fn get_concrete_type_from_impl(&self) -> Result<Ty, Error> {
        let Some(trait_impl) = self.ctx.translated.trait_impls.get(self.impl_ref.id) else {
            raise_error!(
                self.ctx,
                self.span,
                "Trait impl not found: {}",
                self.impl_ref.id.with_ctx(&self.ctx.into_fmt())
            );
        };

        // Get the self type from the trait reference
        // For a trait impl like `impl Trait for ConcreteType`, we want ConcreteType
        let trait_decl_ref = &trait_impl.impl_trait;
        let concrete_ty = &trait_decl_ref.generics.types[0]; // First type arg is Self

        Ok(concrete_ty.clone())
    }

    /// Generate a proper drop shim function instead of using opaque placeholders
    fn generate_drop_shim(&mut self, concrete_ty: &Ty) -> Result<Operand, Error> {
        // Analyze what kind of drop functionality this type has
        let drop_case = self.analyze_drop_case(concrete_ty)?;

        match drop_case {
            DropCase::Found(fun_ref) => {
                // Case 1: Drop function found - generate shim that calls it
                self.create_drop_shim_with_call(&fun_ref, concrete_ty)
            }
            DropCase::NotNeeded => {
                // Case 2: No drop function needed - generate empty shim function
                self.create_empty_drop_shim(concrete_ty)
            }
            DropCase::NotTranslated(msg) => {
                // Case 3: Drop function not translated - generate panic shim + register error
                register_error!(
                    self.ctx,
                    self.span,
                    "Drop function for type {} not found or not translated: {}",
                    self.type_to_string(concrete_ty),
                    msg
                );
                self.create_panic_drop_shim(concrete_ty, &msg)
            }
        }
    }

    /// Create a drop shim that calls the actual drop function (Case 1)
    fn create_drop_shim_with_call(
        &mut self,
        fun_ref: &FunDeclRef,
        concrete_ty: &Ty,
    ) -> Result<Operand, Error> {
        let shim_name = format!("vtable_drop_shim_call_{}_{}", 
            fun_ref.id.index(), 
            self.type_to_string(concrete_ty));
        
        // Create actual function that calls the drop function
        let drop_fn_type = self.create_drop_fn_type(concrete_ty);
        let shim_id = self.create_drop_shim_function(&shim_name, fun_ref.clone(), DropShimKind::CallDrop)?;
        
        // Return function reference as operand
        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(GenericArgs::empty()),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    /// Create an empty drop shim for types that don't need drop (Case 2)  
    fn create_empty_drop_shim(&mut self, concrete_ty: &Ty) -> Result<Operand, Error> {
        let shim_name = format!("vtable_drop_shim_empty_{}", self.type_to_string(concrete_ty));
        
        // Create actual function that just returns
        let drop_fn_type = self.create_drop_fn_type(concrete_ty);
        let dummy_fn_ref = FunDeclRef {
            id: FunDeclId::new(0), // Dummy ID that won't be used
            generics: Box::new(GenericArgs::empty()),
        };
        let shim_id = self.create_drop_shim_function(&shim_name, dummy_fn_ref, DropShimKind::Empty)?;
        
        // Return function reference as operand  
        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(GenericArgs::empty()),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    /// Create a panic drop shim for missing drop functions (Case 3)
    fn create_panic_drop_shim(&mut self, concrete_ty: &Ty, msg: &str) -> Result<Operand, Error> {
        let shim_name = format!("vtable_drop_shim_panic_{}", self.type_to_string(concrete_ty));
        
        // Create actual function that panics
        let drop_fn_type = self.create_drop_fn_type(concrete_ty);
        let dummy_fn_ref = FunDeclRef {
            id: FunDeclId::new(0), // Dummy ID that won't be used
            generics: Box::new(GenericArgs::empty()),
        };
        let shim_id = self.create_drop_shim_function(&shim_name, dummy_fn_ref, DropShimKind::Panic(msg.to_string()))?;
        
        // Return function reference as operand
        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(GenericArgs::empty()),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    /// Compute the size operand for the vtable
    fn compute_size_operand(&self, concrete_ty: &Ty) -> Result<Operand, Error> {
        match self.get_type_size(concrete_ty) {
            Some(size) => {
                let ptr_size = self.ctx.translated.target_information.target_pointer_size;
                match ScalarValue::from_uint(ptr_size, UIntTy::Usize, size) {
                    Ok(scalar_val) => {
                        let size_const = ConstantExpr {
                            value: RawConstantExpr::Literal(Literal::Scalar(scalar_val)),
                            ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
                        };
                        Ok(Operand::Const(Box::new(size_const)))
                    }
                    Err(_) => {
                        // Fall back to opaque if value is out of bounds
                        let opaque_const = ConstantExpr {
                            value: RawConstantExpr::Opaque("size value out of bounds".to_string()),
                            ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
                        };
                        Ok(Operand::Const(Box::new(opaque_const)))
                    }
                }
            }
            None => {
                let opaque_const = ConstantExpr {
                    value: RawConstantExpr::Opaque(
                        "size not available due to generics".to_string(),
                    ),
                    ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
                };
                Ok(Operand::Const(Box::new(opaque_const)))
            }
        }
    }

    /// Compute the align operand for the vtable
    fn compute_align_operand(&self, concrete_ty: &Ty) -> Result<Operand, Error> {
        match self.get_type_align(concrete_ty) {
            Some(align) => {
                let ptr_size = self.ctx.translated.target_information.target_pointer_size;
                match ScalarValue::from_uint(ptr_size, UIntTy::Usize, align) {
                    Ok(scalar_val) => {
                        let align_const = ConstantExpr {
                            value: RawConstantExpr::Literal(Literal::Scalar(scalar_val)),
                            ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
                        };
                        Ok(Operand::Const(Box::new(align_const)))
                    }
                    Err(_) => {
                        // Fall back to opaque if value is out of bounds
                        let opaque_const = ConstantExpr {
                            value: RawConstantExpr::Opaque("align value out of bounds".to_string()),
                            ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
                        };
                        Ok(Operand::Const(Box::new(opaque_const)))
                    }
                }
            }
            None => {
                let opaque_const = ConstantExpr {
                    value: RawConstantExpr::Opaque(
                        "align not available due to generics".to_string(),
                    ),
                    ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
                };
                Ok(Operand::Const(Box::new(opaque_const)))
            }
        }
    }

    /// Analyze what kind of drop case applies to the given concrete type
    fn analyze_drop_case(&self, concrete_ty: &Ty) -> Result<DropCase, Error> {
        match concrete_ty.kind() {
            // For ADT types, check if there's a drop implementation
            TyKind::Adt(type_decl_ref) => {
                if let TypeId::Adt(_type_decl_id) = &type_decl_ref.id {
                    // Look for drop implementations for this type
                    for trait_impl in self.ctx.translated.trait_impls.iter() {
                        // Check if this is a drop implementation for our type
                        let trait_decl_ref = &trait_impl.impl_trait;

                        // Check if this implements the Drop trait
                        if self.is_drop_trait(&trait_decl_ref.id) {
                            // Check if the self type matches our concrete type
                            if let Some(self_ty) = self.get_impl_self_type(trait_impl) {
                                if self.types_match(concrete_ty, &self_ty) {
                                    // Found drop implementation - create function reference
                                    if let Some(drop_method) =
                                        trait_impl.methods.iter().find(|(name, _)| name.0 == "drop")
                                    {
                                        let fun_ref = drop_method.1.skip_binder.clone();
                                        return Ok(DropCase::Found(fun_ref));
                                    }
                                }
                            }
                        }
                    }

                    // No drop implementation found - check if one is needed
                    if self.type_needs_drop(concrete_ty) {
                        Ok(DropCase::NotTranslated(format!(
                            "Drop implementation for {:?} not found or not translated",
                            concrete_ty
                        )))
                    } else {
                        Ok(DropCase::NotNeeded)
                    }
                } else {
                    Ok(DropCase::NotNeeded)
                }
            }

            // For literal types like i32, no drop is needed
            TyKind::Literal(_) => Ok(DropCase::NotNeeded),

            // For other types, conservatively assume no drop is needed for now
            _ => Ok(DropCase::NotNeeded),
        }
    }

    /// Check if a trait declaration is the Drop trait
    fn is_drop_trait(&self, trait_decl_id: &TraitDeclId) -> bool {
        // Look up the trait declaration and check if it has the "drop" lang_item
        if let Some(trait_decl) = self.ctx.translated.trait_decls.get(*trait_decl_id) {
            if let Some(ref lang_item) = trait_decl.item_meta.lang_item {
                return lang_item == "drop";
            }
        }
        false
    }

    /// Get the self type from a trait implementation
    fn get_impl_self_type(&self, trait_impl: &TraitImpl) -> Option<Ty> {
        // For Drop implementations, the Self type is typically found in the implemented trait's generics
        // The Drop trait is `Drop<Self>` so the first generic argument is the Self type
        if let Some(first_generic) = trait_impl.impl_trait.generics.types.get(TypeVarId::new(0)) {
            Some(first_generic.clone())
        } else {
            // If no generic types, try to extract from the trait implementation structure
            // This is a fallback - the actual self type might be encoded elsewhere
            None
        }
    }

    /// Check if two types match for the purpose of drop implementation lookup
    fn types_match(&self, ty1: &Ty, ty2: &Ty) -> bool {
        match (ty1.kind(), ty2.kind()) {
            // ADT types - match by ID
            (TyKind::Adt(ref1), TyKind::Adt(ref2)) => ref1.id == ref2.id,
            // Literal types - exact match
            (TyKind::Literal(lit1), TyKind::Literal(lit2)) => lit1 == lit2,
            // For other types, use structural equality
            _ => ty1 == ty2,
        }
    }

    /// Check if a type needs drop (conservative approach)
    fn type_needs_drop(&self, concrete_ty: &Ty) -> bool {
        match concrete_ty.kind() {
            // Literal types never need drop
            TyKind::Literal(_) => false,
            // ADT types might need drop - check for non-trivial destructors
            TyKind::Adt(type_decl_ref) => {
                if let TypeId::Adt(type_decl_id) = &type_decl_ref.id {
                    if let Some(_type_decl) = self.ctx.translated.type_decls.get(*type_decl_id) {
                        // For now, conservatively assume ADTs don't need drop unless explicitly implemented
                        // This avoids false positives for simple types like structs with only Copy fields
                        false
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            // Other types conservatively assumed not to need drop
            _ => false,
        }
    }

    /// Get the size of a type from its layout information
    fn get_type_size(&self, ty: &Ty) -> Option<u128> {
        match ty.kind() {
            TyKind::Adt(type_decl_ref) => {
                let TypeId::Adt(type_decl_id) = type_decl_ref.id else {
                    return None;
                };
                let type_decl = self.ctx.translated.type_decls.get(type_decl_id)?;
                type_decl.layout.as_ref()?.size?.try_into().ok()
            }
            TyKind::Literal(lit_ty) => {
                // For literal types, we can compute size directly
                Some(match lit_ty {
                    LiteralTy::Bool => 1,
                    LiteralTy::Char => 4,
                    LiteralTy::Int(int_ty) => self.get_int_size(*int_ty),
                    LiteralTy::UInt(uint_ty) => self.get_uint_size(*uint_ty),
                    LiteralTy::Float(float_ty) => match float_ty {
                        FloatTy::F16 => 2,
                        FloatTy::F32 => 4,
                        FloatTy::F64 => 8,
                        FloatTy::F128 => 16,
                    },
                })
            }
            _ => None, // For complex types like references, generics, etc., we can't determine size here
        }
    }

    /// Get the alignment of a type from its layout information  
    fn get_type_align(&self, ty: &Ty) -> Option<u128> {
        match ty.kind() {
            TyKind::Adt(type_decl_ref) => {
                let TypeId::Adt(type_decl_id) = type_decl_ref.id else {
                    return None;
                };
                let type_decl = self.ctx.translated.type_decls.get(type_decl_id)?;
                type_decl.layout.as_ref()?.align?.try_into().ok()
            }
            TyKind::Literal(lit_ty) => {
                // For literal types, alignment is typically the same as size (with some exceptions)
                Some(match lit_ty {
                    LiteralTy::Bool => 1,
                    LiteralTy::Char => 4,
                    LiteralTy::Int(int_ty) => self.get_int_size(*int_ty),
                    LiteralTy::UInt(uint_ty) => self.get_uint_size(*uint_ty),
                    LiteralTy::Float(float_ty) => match float_ty {
                        FloatTy::F16 => 2,
                        FloatTy::F32 => 4,
                        FloatTy::F64 => 8,
                        FloatTy::F128 => 16, // May need special handling
                    },
                })
            }
            _ => None,
        }
    }

    /// Get the size of an integer type
    fn get_int_size(&self, int_ty: IntTy) -> u128 {
        match int_ty {
            IntTy::Isize => self
                .ctx
                .translated
                .target_information
                .target_pointer_size
                .try_into()
                .unwrap(),
            IntTy::I8 => 1,
            IntTy::I16 => 2,
            IntTy::I32 => 4,
            IntTy::I64 => 8,
            IntTy::I128 => 16,
        }
    }

    /// Get the size of an unsigned integer type
    fn get_uint_size(&self, uint_ty: UIntTy) -> u128 {
        match uint_ty {
            UIntTy::Usize => self
                .ctx
                .translated
                .target_information
                .target_pointer_size
                .try_into()
                .unwrap(),
            UIntTy::U8 => 1,
            UIntTy::U16 => 2,
            UIntTy::U32 => 4,
            UIntTy::U64 => 8,
            UIntTy::U128 => 16,
        }
    }

    /// Convert a type to a string representation for display purposes
    fn type_to_string(&self, ty: &Ty) -> String {
        match ty.kind() {
            TyKind::Literal(lit_ty) => match lit_ty {
                LiteralTy::Bool => "bool".to_string(),
                LiteralTy::Char => "char".to_string(),
                LiteralTy::Int(int_ty) => format!("{:?}", int_ty).to_lowercase(),
                LiteralTy::UInt(uint_ty) => format!("{:?}", uint_ty).to_lowercase(),
                LiteralTy::Float(float_ty) => format!("{:?}", float_ty).to_lowercase(),
            },
            TyKind::Adt(type_decl_ref) => {
                format!("adt_{:?}", type_decl_ref.id)
            }
            _ => format!("{:?}", ty).chars().take(50).collect(),
        }
    }

    /// Create the function pointer type for a drop function: `fn(*mut dyn Trait)`
    fn create_drop_fn_type(&self, concrete_ty: &Ty) -> Ty {
        // TODO: Create the correct dyn trait type based on the concrete type
        // For now, use a placeholder
        let mut_ptr_ty = TyKind::RawPtr(concrete_ty.clone(), RefKind::Mut).into_ty();
        let fn_sig = RegionBinder::empty((vec![mut_ptr_ty], Ty::mk_unit()));
        TyKind::FnPtr(fn_sig).into_ty()
    }

    /// Different kinds of drop shims we need to generate
    fn create_drop_shim_function(
        &mut self,
        name: &str,
        drop_fn_ref: FunDeclRef,
        kind: DropShimKind,
    ) -> Result<FunDeclId, Error> {
        // Create a self parameter type (mutable pointer to trait object)
        let self_ty = TyKind::RawPtr(
            Ty::mk_unit(), // Placeholder - should be proper trait object type
            RefKind::Mut
        ).into_ty();

        // Create function signature
        let signature = FunSig {
            is_unsafe: false,
            generics: GenericParams::empty(),
            inputs: vec![self_ty.clone()],
            output: Ty::mk_unit(),
        };

        // Create locals for the function
        let mut locals = Locals::default();
        locals.arg_count = 1; // One argument: &mut self

        // Return value (unit)  
        let ret_local = Local {
            index: LocalId::new(0),
            name: Some("ret".to_string()),
            ty: Ty::mk_unit(),
        };

        // Self parameter
        let self_local = Local {
            index: LocalId::new(1), 
            name: Some("self".to_string()),
            ty: self_ty.clone(),
        };

        let _ = locals.locals.push_with(|_| ret_local);
        let _ = locals.locals.push_with(|_| self_local);

        // Create function body based on the kind
        let body = match kind {
            DropShimKind::CallDrop => {
                // Generate a call to the drop function
                let mut blocks = Vector::new();
                
                let call = Call {
                    func: FnOperand::Regular(drop_fn_ref.into()),
                    args: vec![Operand::Copy(Place::new(LocalId::new(1), self_ty.clone()))],
                    dest: Place::new(LocalId::new(0), Ty::mk_unit()),
                };

                let terminator = Terminator {
                    span: self.span,
                    content: RawTerminator::Call {
                        call,
                        target: BlockId::new(1),
                        on_unwind: BlockId::new(1), // Same as target for simplicity
                    },
                    comments_before: vec![],
                };

                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator,
                });

                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec![],
                    },
                });

                Body::Unstructured(ExprBody {
                    span: self.span,
                    locals: locals.clone(),
                    comments: vec![],
                    body: blocks,
                })
            }
            DropShimKind::Empty => {
                // Generate an empty function that just returns
                let mut blocks = Vector::new();
                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec![],
                    },
                });

                Body::Unstructured(ExprBody {
                    span: self.span,
                    locals: locals.clone(),
                    comments: vec![],
                    body: blocks,
                })
            }
            DropShimKind::Panic(msg) => {
                // Generate a function that panics
                let mut blocks = Vector::new();
                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Abort(AbortKind::Panic(None)),
                        comments_before: vec![format!("Panic: {}", msg)],
                    },
                });

                Body::Unstructured(ExprBody {
                    span: self.span,
                    locals: locals.clone(), 
                    comments: vec![],
                    body: blocks,
                })
            }
        };

        // Create item meta
        let item_meta = ItemMeta {
            span: self.span,
            name: Name::from_path(&[name]),
            source_text: None,
            attr_info: AttrInfo::default(),
            is_local: true,
            opacity: ItemOpacity::Opaque, // Mark as opaque since it's a generated shim
            lang_item: None,
        };

        // Create and add function declaration  
        let shim_id = self.ctx.translated.fun_decls.push_with(|id| FunDecl {
            def_id: id,
            item_meta,
            signature,
            kind: ItemKind::TopLevel,
            is_global_initializer: None,
            body: Ok(body),
        });

        Ok(shim_id)
    }
}

/// Different kinds of drop shims we need to generate
#[derive(Debug, Clone)]
enum DropShimKind {
    /// Call the actual drop function
    CallDrop,
    /// Empty function (no drop needed)
    Empty,
    /// Panic function (drop not translated)
    Panic(String),
}

/// Represents the three cases for drop function resolution
#[derive(Debug)]
enum DropCase {
    /// Case 1: Drop function found - contains the function reference
    Found(FunDeclRef),
    /// Case 2: No drop function needed (e.g., i32)
    NotNeeded,
    /// Case 3: Drop function not translated - contains error message
    NotTranslated(String),
}

/// Count vtable instances for logging
fn count_vtable_instances(ctx: &TransformCtx) -> usize {
    ctx.translated
        .fun_decls
        .iter()
        .filter(|decl| matches!(decl.kind, ItemKind::VTableInstance { .. }))
        .count()
}
