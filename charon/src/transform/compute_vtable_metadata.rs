//! Compute metadata for vtable instances (size, align, drop).
//!
//! This pass fills the metadata fields of vtable instances with correct values instead of opaque placeholders.
//!
//! For each vtable instance initializer function, we:
//! 1. Extract the concrete type being implemented for
//! 2. Compute size & align from the type's layout information
//! 3. Generate proper drop shim functions for the drop field
//! 4. Replace the opaque placeholders with the actual values

use either::Either::{self};

use super::ctx::TransformPass;
use crate::{
    ast::ScalarValue, errors::Error, formatter::IntoFormatter, pretty::FmtWithCtx, raise_error,
    register_error, transform::TransformCtx, ullbc_ast::*,
};

/// Vtable metadata computer that holds common state and provides methods
/// for computing size, align, and drop shim functions for vtable instances.
struct VtableMetadataComputer<'a> {
    ctx: &'a mut TransformCtx,
    impl_ref: &'a TraitImplRef,
    span: Span,
    /// The type of the drop field: `fn<'a>(self: &'a mut dyn Trait<...>)`
    drop_field_ty: Option<Ty>,
}

impl<'a> VtableMetadataComputer<'a> {
    fn new(ctx: &'a mut TransformCtx, impl_ref: &'a TraitImplRef, span: Span) -> Self {
        Self {
            ctx,
            impl_ref,
            span,
            drop_field_ty: None,
        }
    }

    // ========================================
    // MAIN COMPUTATION ENTRY POINTS
    // ========================================

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
        self.drop_field_ty = Some(fields[2].ty().clone());

        // Get the concrete type from the impl
        let concrete_ty = self.get_concrete_type_from_impl()?;

        // Update both size & align field with the info of the concrete type
        self.compute_layout(fields, &concrete_ty)?;

        // Update drop field - generate actual shim function instead of opaque
        fields[2] = self.generate_drop_shim(&concrete_ty)?;

        Ok(())
    }

    fn compute_layout(&mut self, fields: &mut Vec<Operand>, concrete_ty: &Ty) -> Result<(), Error> {
        match concrete_ty.layout(&self.ctx.translated) {
            Ok(layout) => {
                fields[0] = self.layout_field_constant(match layout.size {
                    Some(size) => Either::Right(size),
                    None => Either::Left("Size not available".to_string()),
                })?;
                fields[1] = self.layout_field_constant(match layout.align {
                    Some(align) => Either::Right(align),
                    None => Either::Left("Align not available".to_string()),
                })?;
            }
            Err(reason) => {
                // Fallback to opaque values for both size and align when layout computation fails
                let reason_msg = format!("Layout not available: {}", reason);
                fields[0] = self.layout_field_constant(Either::Left(reason_msg.clone()))?;
                fields[1] = self.layout_field_constant(Either::Left(reason_msg))?;
            }
        }
        Ok(())
    }

    /// Create a constant operand for layout fields (size/align)
    /// If Left(reason), creates an opaque constant with the reason
    /// If Right(value), creates a literal constant with the value
    fn layout_field_constant(&self, value: Either<String, u64>) -> Result<Operand, Error> {
        match value {
            Either::Left(reason) => Ok(Operand::opaque(reason, Ty::mk_usize())),
            Either::Right(val) => {
                let expr = ConstantExpr {
                    value: RawConstantExpr::Literal(Literal::Scalar(
                        ScalarValue::from_uint(
                            self.ctx.translated.target_information.target_pointer_size,
                            UIntTy::Usize,
                            val as u128,
                        )
                        .or_else(|_| {
                            raise_error!(self.ctx, self.span, "Layout value out of bounds")
                        })?,
                    )),
                    ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
                };
                Ok(Operand::Const(Box::new(expr)))
            }
        }
    }

    // ========================================
    // VTABLE DETECTION AND TYPE EXTRACTION
    // ========================================

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

    // ========================================
    // DROP SHIM GENERATION
    // ========================================

    /// Generate a proper drop shim function instead of using opaque placeholders
    fn generate_drop_shim(&mut self, concrete_ty: &Ty) -> Result<Operand, Error> {
        // Analyze what kind of drop functionality this type has
        let drop_case = self.analyze_drop_case(concrete_ty)?;

        match drop_case {
            DropCase::Found(drop_found) => {
                // Case 1: Drop function found - generate appropriate shim based on type
                self.create_drop_shim_for_found(drop_found, concrete_ty)
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
            DropCase::Unknown(msg) => Ok(Operand::opaque(msg, self.get_drop_field_ty()?.clone())),
        }
    }

    fn get_drop_field_ty(&self) -> Result<&Ty, Error> {
        match self.drop_field_ty {
            Some(ref ty) => Ok(ty),
            None => raise_error!(self.ctx, self.span, "Drop field type not initialized"),
        }
    }

    /// Create drop shim based on the specific DropFound case
    fn create_drop_shim_for_found(&mut self, drop_found: DropFound, concrete_ty: &Ty) -> Result<Operand, Error> {
        match drop_found {
            DropFound::Direct(fun_ref) => {
                // Simple case: direct drop implementation
                self.create_drop_shim_with_call(&fun_ref, concrete_ty)
            }
            DropFound::Array { element_ty, element_drop } => {
                // Generate array traversal drop shim
                self.create_array_drop_shim(concrete_ty, &element_ty, *element_drop)
            }
            DropFound::Tuple { fields } => {
                // Generate tuple field-by-field drop shim
                self.create_tuple_drop_shim(concrete_ty, fields)
            }
            DropFound::Box { inner_ty, inner_drop } => {
                // Generate Box drop shim
                self.create_box_drop_shim(concrete_ty, &inner_ty, *inner_drop)
            }
        }
    }

    /// Create a drop shim that calls the actual drop function (Case 1)
    fn create_drop_shim_with_call(
        &mut self,
        fun_ref: &FunDeclRef,
        concrete_ty: &Ty,
    ) -> Result<Operand, Error> {
        let shim_name = format!(
            "{{vtable_drop_shim_call_{}_{}}}",
            fun_ref.id.index(),
            self.type_to_string(concrete_ty)
        );

        // Create the drop shim function
        let shim_id = self.create_drop_shim_function(
            &shim_name,
            fun_ref.clone(),
            DropShimKind::CallDrop,
            concrete_ty,
        )?;

        // Create a simple function type that matches our drop shim function
        let dyn_trait_param_ty = self.get_drop_receiver()?;
        let fn_sig = RegionBinder::empty((vec![dyn_trait_param_ty], Ty::mk_unit()));
        let drop_fn_type = TyKind::FnPtr(fn_sig).into_ty();

        // Return function reference as operand
        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(self.create_drop_shim_function_generics()?),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    /// Create an empty drop shim for types that don't need drop (Case 2)  
    fn create_empty_drop_shim(&mut self, concrete_ty: &Ty) -> Result<Operand, Error> {
        let shim_name = format!(
            "{{vtable_drop_shim_empty_{}}}",
            self.type_to_string(concrete_ty)
        );

        // Create the drop shim function
        let dummy_fn_ref = FunDeclRef {
            id: FunDeclId::new(0), // Dummy ID that won't be used
            generics: Box::new(GenericArgs::empty()),
        };
        let shim_id = self.create_drop_shim_function(
            &shim_name,
            dummy_fn_ref,
            DropShimKind::Empty,
            concrete_ty,
        )?;

        // Create a simple function type that matches our drop shim function
        let dyn_trait_param_ty = self.get_drop_receiver()?;
        let fn_sig = RegionBinder::empty((vec![dyn_trait_param_ty], Ty::mk_unit()));
        let drop_fn_type = TyKind::FnPtr(fn_sig).into_ty();

        // Return function reference as operand
        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(self.create_drop_shim_function_generics()?),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    /// Create a panic drop shim for missing drop functions (Case 3)
    fn create_panic_drop_shim(&mut self, concrete_ty: &Ty, msg: &str) -> Result<Operand, Error> {
        let shim_name = format!(
            "{{vtable_drop_shim_panic_{}}}",
            self.type_to_string(concrete_ty)
        );

        // Create the drop shim function
        let dummy_fn_ref = FunDeclRef {
            id: FunDeclId::new(0), // Dummy ID that won't be used
            generics: Box::new(GenericArgs::empty()),
        };
        let shim_id = self.create_drop_shim_function(
            &shim_name,
            dummy_fn_ref,
            DropShimKind::Panic(msg.to_string()),
            concrete_ty,
        )?;

        // Create a simple function type that matches our drop shim function
        let dyn_trait_param_ty = self.get_drop_receiver()?;
        let fn_sig = RegionBinder::empty((vec![dyn_trait_param_ty], Ty::mk_unit()));
        let drop_fn_type = TyKind::FnPtr(fn_sig).into_ty();

        // Return function reference as operand
        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(self.create_drop_shim_function_generics()?),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    // ========================================
    // SPECIALIZED DROP SHIM CREATION METHODS  
    // ========================================

    /// Create a drop shim for array types that traverses and drops each element
    fn create_array_drop_shim(&mut self, concrete_ty: &Ty, element_ty: &Ty, element_drop: DropCase) -> Result<Operand, Error> {
        let shim_name = format!(
            "{{vtable_drop_shim_array_{}_elem_{}}}",
            self.type_to_string(concrete_ty),
            self.type_to_string(element_ty)
        );

        // Create the drop shim function
        let dummy_fn_ref = FunDeclRef {
            id: FunDeclId::new(0), // Dummy ID that won't be used
            generics: Box::new(GenericArgs::empty()),
        };
        let shim_id = self.create_drop_shim_function(
            &shim_name,
            dummy_fn_ref,
            DropShimKind::ArrayTraversal {
                element_ty: element_ty.clone(),
                element_drop: element_drop,
            },
            concrete_ty,
        )?;

        // Create function reference as operand
        let dyn_trait_param_ty = self.get_drop_receiver()?;
        let fn_sig = RegionBinder::empty((vec![dyn_trait_param_ty], Ty::mk_unit()));
        let drop_fn_type = TyKind::FnPtr(fn_sig).into_ty();

        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(self.create_drop_shim_function_generics()?),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    /// Create a drop shim for tuple types that drops each field needing drop
    fn create_tuple_drop_shim(&mut self, concrete_ty: &Ty, fields: Vec<(usize, Ty, DropCase)>) -> Result<Operand, Error> {
        let shim_name = format!(
            "{{vtable_drop_shim_tuple_{}_fields_{}}}",
            self.type_to_string(concrete_ty),
            fields.len()
        );

        // Create the drop shim function
        let dummy_fn_ref = FunDeclRef {
            id: FunDeclId::new(0), // Dummy ID that won't be used
            generics: Box::new(GenericArgs::empty()),
        };
        let shim_id = self.create_drop_shim_function(
            &shim_name,
            dummy_fn_ref,
            DropShimKind::TupleFields { fields },
            concrete_ty,
        )?;

        // Create function reference as operand
        let dyn_trait_param_ty = self.get_drop_receiver()?;
        let fn_sig = RegionBinder::empty((vec![dyn_trait_param_ty], Ty::mk_unit()));
        let drop_fn_type = TyKind::FnPtr(fn_sig).into_ty();

        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(self.create_drop_shim_function_generics()?),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    /// Create a drop shim for Box types
    fn create_box_drop_shim(&mut self, concrete_ty: &Ty, inner_ty: &Ty, inner_drop: DropCase) -> Result<Operand, Error> {
        let shim_name = format!(
            "{{vtable_drop_shim_box_{}_inner_{}}}",
            self.type_to_string(concrete_ty),
            self.type_to_string(inner_ty)
        );

        // Create the drop shim function
        let dummy_fn_ref = FunDeclRef {
            id: FunDeclId::new(0), // Dummy ID that won't be used
            generics: Box::new(GenericArgs::empty()),
        };
        let shim_id = self.create_drop_shim_function(
            &shim_name,
            dummy_fn_ref,
            DropShimKind::BoxDrop {
                inner_ty: inner_ty.clone(),
                inner_drop: inner_drop,
            },
            concrete_ty,
        )?;

        // Create function reference as operand
        let dyn_trait_param_ty = self.get_drop_receiver()?;
        let fn_sig = RegionBinder::empty((vec![dyn_trait_param_ty], Ty::mk_unit()));
        let drop_fn_type = TyKind::FnPtr(fn_sig).into_ty();

        let shim_const = ConstantExpr {
            value: RawConstantExpr::FnPtr(FnPtr::from(FunDeclRef {
                id: shim_id,
                generics: Box::new(self.create_drop_shim_function_generics()?),
            })),
            ty: drop_fn_type,
        };
        Ok(Operand::Const(Box::new(shim_const)))
    }

    // ========================================
    // DROP CASE ANALYSIS
    // ========================================

    /// Analyze what kind of drop case applies to the given concrete type
    fn analyze_drop_case(&self, concrete_ty: &Ty) -> Result<DropCase, Error> {
        // First check if the type needs drop at all
        match concrete_ty.needs_drop(&self.ctx.translated) {
            Ok(false) => return Ok(DropCase::NotNeeded),
<<<<<<< Updated upstream
            Ok(true) => { } // Continue to analyze the specific drop case
=======
            Ok(true) => {} // Continue to check for drop implementation
>>>>>>> Stashed changes
            Err(reason) => return Ok(DropCase::Unknown(reason)),
        }

        // Analyze the specific type to determine how to handle dropping
        match concrete_ty.kind() {
            TyKind::Adt(type_decl_ref) => {
                match &type_decl_ref.id {
                    TypeId::Adt(_type_decl_id) => {
                        // Look for direct drop implementation for ADT types
                        if let Some(fun_ref) = self.find_direct_drop_impl(concrete_ty)? {
                            Ok(DropCase::Found(DropFound::Direct(fun_ref)))
                        } else {
                            Ok(DropCase::NotTranslated(format!(
                                "Drop implementation for {:?} not found or not translated",
                                concrete_ty
                            )))
                        }
                    }
                    TypeId::Tuple => {
                        // Handle tuple drop - analyze each field
                        self.analyze_tuple_drop(type_decl_ref)
                    }
                    TypeId::Builtin(builtin_ty) => {
                        match builtin_ty {
                            BuiltinTy::Box => {
                                // Handle Box drop
                                self.analyze_box_drop(type_decl_ref)
                            }
                            BuiltinTy::Array => {
                                // Handle array drop - traverse and drop each element
                                self.analyze_array_drop(type_decl_ref)
                            }
                            BuiltinTy::Slice => {
                                // Handle slice drop - similar to array
                                self.analyze_slice_drop(type_decl_ref)
                            }
                            BuiltinTy::Str => Ok(DropCase::NotNeeded), // str does not need drop
                        }
                    }
                }
            }

            TyKind::Literal(_) => Ok(DropCase::NotNeeded),
<<<<<<< Updated upstream
            TyKind::Ref(..) | TyKind::RawPtr(..) => Ok(DropCase::NotNeeded), // References and raw pointers don't need drop
            
            TyKind::TraitType(..) |
            TyKind::DynTrait(..) |
            TyKind::FnPtr(..) |
            TyKind::FnDef(..) |
            TyKind::Never |
            TyKind::TypeVar(..) |
            TyKind::Error(..) => Ok(DropCase::Unknown(format!("Unknown Drop for type: {}", self.type_to_string(concrete_ty)))),
=======

            TyKind::Ref(..) | TyKind::RawPtr(..) => Ok(DropCase::NotNeeded), // References and raw pointers don't need drop

            TyKind::TraitType(..)
            | TyKind::DynTrait(..)
            | TyKind::FnPtr(..)
            | TyKind::FnDef(..)
            | TyKind::Never
            | TyKind::TypeVar(..)
            | TyKind::Error(..) => Ok(DropCase::Unknown(format!(
                "Unknown Drop for type: {}",
                self.type_to_string(concrete_ty)
            ))),
>>>>>>> Stashed changes
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
    fn type_needs_drop(&self, concrete_ty: &Ty) -> Result<bool, Error> {
        match concrete_ty.needs_drop(&self.ctx.translated) {
            Ok(needs_drop) => Ok(needs_drop),
            Err(reason) => raise_error!(self.ctx, self.span, "{}", reason),
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

    // ========================================
    // NEW DROP CASE ANALYSIS METHODS
    // ========================================

    /// Find direct drop implementation for a concrete type
    fn find_direct_drop_impl(&self, concrete_ty: &Ty) -> Result<Option<FunDeclRef>, Error> {
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
                            return Ok(Some(fun_ref));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    /// Analyze drop case for tuple types
    fn analyze_tuple_drop(&self, type_decl_ref: &TypeDeclRef) -> Result<DropCase, Error> {
        let tuple_generics = &type_decl_ref.generics.types;
        let mut fields = Vec::new();
        
        // Analyze each field in the tuple
        for (field_index, field_ty) in tuple_generics.iter().enumerate() {
            let field_drop_case = self.analyze_drop_case(field_ty)?;
            
            // Only include fields that need dropping
            match field_drop_case {
                DropCase::NotNeeded => continue, // Skip fields that don't need drop
                _ => fields.push((field_index, field_ty.clone(), field_drop_case)),
            }
        }

        if fields.is_empty() {
            // No fields need dropping
            Ok(DropCase::NotNeeded)
        } else {
            // Some fields need dropping - generate composite drop
            Ok(DropCase::Found(DropFound::Tuple { fields }))
        }
    }

    /// Analyze drop case for Box types
    fn analyze_box_drop(&self, type_decl_ref: &TypeDeclRef) -> Result<DropCase, Error> {
        // Box<T> always needs drop, but we need to determine the drop strategy for T
        if let Some(inner_ty) = type_decl_ref.generics.types.get(TypeVarId::new(0)) {
            let inner_drop_case = self.analyze_drop_case(inner_ty)?;
            Ok(DropCase::Found(DropFound::Box {
                inner_ty: inner_ty.clone(),
                inner_drop: Box::new(inner_drop_case),
            }))
        } else {
            Ok(DropCase::Unknown("Box type missing inner type parameter".to_string()))
        }
    }

    /// Analyze drop case for array types [T; N]
    fn analyze_array_drop(&self, type_decl_ref: &TypeDeclRef) -> Result<DropCase, Error> {
        // Array [T; N] needs drop if T needs drop
        if let Some(element_ty) = type_decl_ref.generics.types.get(TypeVarId::new(0)) {
            let element_drop_case = self.analyze_drop_case(element_ty)?;
            match element_drop_case {
                DropCase::NotNeeded => Ok(DropCase::NotNeeded),
                _ => Ok(DropCase::Found(DropFound::Array {
                    element_ty: element_ty.clone(),
                    element_drop: Box::new(element_drop_case),
                })),
            }
        } else {
            Ok(DropCase::Unknown("Array type missing element type parameter".to_string()))
        }
    }

    /// Analyze drop case for slice types &[T]
    fn analyze_slice_drop(&self, type_decl_ref: &TypeDeclRef) -> Result<DropCase, Error> {
        // Slice &[T] needs drop if T needs drop (similar to array)
        if let Some(element_ty) = type_decl_ref.generics.types.get(TypeVarId::new(0)) {
            let element_drop_case = self.analyze_drop_case(element_ty)?;
            match element_drop_case {
                DropCase::NotNeeded => Ok(DropCase::NotNeeded),
                _ => Ok(DropCase::Found(DropFound::Array {
                    element_ty: element_ty.clone(),
                    element_drop: Box::new(element_drop_case),
                })),
            }
        } else {
            Ok(DropCase::Unknown("Slice type missing element type parameter".to_string()))
        }
    }

    // ========================================
    // FUNCTION TYPE AND GENERICS CREATION
    // ========================================
    // FUNCTION CREATION AND BODY GENERATION
    // ========================================

    /// Different kinds of drop shims we need to generate
    fn create_drop_shim_function(
        &mut self,
        name: &str,
        drop_fn_ref: FunDeclRef,
        kind: DropShimKind,
        concrete_ty: &Ty,
    ) -> Result<FunDeclId, Error> {
        // Get the generics from the trait impl - drop shims should have the same generics
        let generics = self.get_trait_impl_generics()?;

        // Create the dyn trait type for the parameter
        // For the drop shim, we need *mut (dyn Trait<...>)
        let dyn_trait_param_ty = self.get_drop_receiver()?;

        // Create function signature with proper generics
        let signature = FunSig {
            is_unsafe: false,
            generics,
            inputs: vec![dyn_trait_param_ty.clone()],
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

        // Self parameter (dyn trait object pointer)
        let self_local = Local {
            index: LocalId::new(1),
            name: Some("self".to_string()),
            ty: dyn_trait_param_ty.clone(),
        };

        let _ = locals.locals.push_with(|_| ret_local);
        let _ = locals.locals.push_with(|_| self_local);

        // For CallDrop case, add a local for the concretized concrete type
        let mut concrete_local_id = None;
        if matches!(kind, DropShimKind::CallDrop) {
            // Create the concrete type (mutable reference to the concrete type)
            let concrete_ref_ty =
                TyKind::Ref(Region::Erased, concrete_ty.clone(), RefKind::Mut).into_ty();

            let concrete_local = Local {
                index: LocalId::new(2),
                name: Some("concrete".to_string()),
                ty: concrete_ref_ty,
            };

            concrete_local_id = Some(locals.locals.push_with(|_| concrete_local));
            locals.arg_count = 1; // Still only one argument
        }

        // Create function body based on the kind
        let body = match kind {
            DropShimKind::CallDrop => {
                // Generate a call to the drop function with Concretize cast
                let mut blocks = Vector::new();

                // Create the concretize cast statement
                let Some(concrete_local_id) = concrete_local_id else {
                    raise_error!(
                        self.ctx,
                        self.span,
                        "Expected concrete local to be created for CallDrop case"
                    );
                };

                // Get the concrete type for the concretize cast
                let concrete_ref_ty =
                    TyKind::Ref(Region::Erased, concrete_ty.clone(), RefKind::Mut).into_ty();

                let concretize_stmt = Statement {
                    span: self.span,
                    content: RawStatement::Assign(
                        Place::new(concrete_local_id, concrete_ref_ty.clone()),
                        Rvalue::UnaryOp(
                            UnOp::Cast(CastKind::Concretize(
                                dyn_trait_param_ty.clone(),
                                concrete_ref_ty.clone(),
                            )),
                            Operand::Move(Place::new(LocalId::new(1), dyn_trait_param_ty.clone())),
                        ),
                    ),
                    comments_before: vec![],
                };

                let call = Call {
                    func: FnOperand::Regular(FnPtr::from(FunDeclRef {
                        id: drop_fn_ref.id,
                        generics: Box::new(self.create_drop_function_generics()?),
                    })),
                    args: vec![Operand::Move(Place::new(
                        concrete_local_id,
                        concrete_ref_ty,
                    ))],
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
                    statements: vec![concretize_stmt],
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
            DropShimKind::ArrayTraversal { element_ty, element_drop: _ } => {
                // Generate array traversal drop logic
                // For now, use a placeholder implementation that just returns
                // TODO: Implement proper array traversal with loop and element dropping
                let mut blocks = Vector::new();
                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec![format!("TODO: Array traversal drop for element type: {:?}", element_ty)],
                    },
                });

                Body::Unstructured(ExprBody {
                    span: self.span,
                    locals: locals.clone(),
                    comments: vec![],
                    body: blocks,
                })
            }
            DropShimKind::TupleFields { fields } => {
                // Generate tuple field-by-field drop logic
                // For now, use a placeholder implementation that just returns
                // TODO: Implement proper tuple field dropping
                let mut blocks = Vector::new();
                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec![format!("TODO: Tuple field drop for {} fields", fields.len())],
                    },
                });

                Body::Unstructured(ExprBody {
                    span: self.span,
                    locals: locals.clone(),
                    comments: vec![],
                    body: blocks,
                })
            }
            DropShimKind::BoxDrop { inner_ty, inner_drop: _ } => {
                // Generate Box drop logic
                // For now, use a placeholder implementation that just returns
                // TODO: Implement proper Box drop
                let mut blocks = Vector::new();
                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec![format!("TODO: Box drop for inner type: {:?}", inner_ty)],
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
            opacity: ItemOpacity::Transparent, // Mark as transparent so the name shows up
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

    /// Get the generics from the trait impl that should be used for drop shim functions
    fn get_trait_impl_generics(&self) -> Result<GenericParams, Error> {
        let Some(trait_impl) = self.ctx.translated.trait_impls.get(self.impl_ref.id) else {
            raise_error!(
                self.ctx,
                self.span,
                "Trait impl not found: {}",
                self.impl_ref.id.with_ctx(&self.ctx.into_fmt())
            );
        };

        // Drop shim functions should have the same generic parameters as the trait impl
        Ok(trait_impl.generics.clone())
    }

    /// The `&'_ mut (dyn Trait<...>)` receiver, where the lifetime is erased
    /// Should be re-constructed in the drop shims
    fn get_drop_receiver(&self) -> Result<Ty, Error> {
        match self.drop_field_ty {
            Some(ref ty) => match ty {
                TyKind::FnPtr(binded_sig) => Ok(binded_sig.clone().erase().0[0].clone()),
                _ => unreachable!(),
            },
            None => raise_error!(self.ctx, self.span, "Uninitialized drop field ty!"),
        }
    }

    /// Create the generic arguments to be used when calling the drop function
    /// This should match the generics from the drop function signature (typically just one lifetime)
    fn create_drop_function_generics(&self) -> Result<GenericArgs, Error> {
        // Drop functions typically have a signature like: fn drop<'a>(&'a mut self)
        // So they expect exactly one region parameter for the mutable reference lifetime
        let drop_generics = GenericArgs {
            regions: Vector::from_iter(vec![Region::Erased]), // One erased region for the &mut self lifetime
            types: Vector::new(), // Drop functions don't take type parameters
            const_generics: Vector::new(), // Drop functions don't take const generics
            trait_refs: Vector::new(), // Drop functions don't take trait references
        };

        Ok(drop_generics)
    }

    /// Create the generic arguments for referencing the drop shim function itself
    /// This should include all the generics that the drop shim function was defined with
    fn create_drop_shim_function_generics(&self) -> Result<GenericArgs, Error> {
        // The drop shim function reference should use the same generic arguments as the impl_ref
        // Use the generic arguments from the trait impl ref directly
        Ok(*self.impl_ref.generics.clone())
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
    /// Array traversal drop (drop each element)
    ArrayTraversal {
        element_ty: Ty,
        element_drop: DropCase,
    },
    /// Tuple field-by-field drop (drop each field that needs it)
    TupleFields {
        fields: Vec<(usize, Ty, DropCase)>,
    },
    /// Box drop (drop the contained value and then the box itself)
    BoxDrop {
        inner_ty: Ty,
        inner_drop: DropCase,
    },
}

/// Represents the different cases for drop function resolution
#[derive(Debug, Clone)]
enum DropCase {
    /// Case 1: Drop function found - contains the function reference
    Found(DropFound),
    /// Case 2: No drop function needed (e.g., i32)
    NotNeeded,
    /// Case 3: Drop function not translated - contains error message
    NotTranslated(String),
    /// Case 4: Unknown due to generics
    Unknown(String),
}

/// Sub-cases for when a drop function is found or needs to be generated
#[derive(Debug, Clone)]
enum DropFound {
    /// Simple case: Direct drop implementation available
    Direct(FunDeclRef),
    /// Array case: Generate shim to traverse and drop each element
    Array {
        element_ty: Ty,
        element_drop: Box<DropCase>,
    },
    /// Tuple case: Generate shim to drop each field that needs drop
    Tuple {
        fields: Vec<(usize, Ty, DropCase)>, // (field_index, field_type, field_drop_case)
    },
    /// Box case: Generate shim for Box drop
    Box {
        inner_ty: Ty,
        inner_drop: Box<DropCase>,
    },
}

/// Count vtable instances for logging
fn count_vtable_instances(ctx: &TransformCtx) -> usize {
    ctx.translated
        .fun_decls
        .iter()
        .filter(|decl| matches!(decl.kind, ItemKind::VTableInstance { .. }))
        .count()
}

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
