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

// ========================================
// HELPER STRUCTURES
// ========================================

/// Helper for creating constants with specific types
struct ConstantBuilder<'a> {
    ctx: &'a TransformCtx,
    span: Span,
}

impl<'a> ConstantBuilder<'a> {
    fn new(ctx: &'a TransformCtx, span: Span) -> Self {
        Self { ctx, span }
    }

    fn zero_constant(&self, ty: &Ty) -> Result<ConstantExpr, Error> {
        match ty.kind() {
            TyKind::Literal(LiteralTy::UInt(uint_ty)) => {
                let expr = ConstantExpr {
                    value: RawConstantExpr::Literal(Literal::Scalar(
                        ScalarValue::from_uint(
                            self.ctx.translated.target_information.target_pointer_size,
                            *uint_ty,
                            0,
                        )
                        .or_else(|_| {
                            raise_error!(self.ctx, self.span, "Zero value out of bounds")
                        })?,
                    )),
                    ty: ty.clone(),
                };
                Ok(expr)
            }
            _ => raise_error!(
                self.ctx,
                self.span,
                "Unsupported type for zero constant: {:?}",
                ty
            ),
        }
    }

    fn one_constant(&self, ty: &Ty) -> Result<ConstantExpr, Error> {
        match ty.kind() {
            TyKind::Literal(LiteralTy::UInt(uint_ty)) => {
                let expr = ConstantExpr {
                    value: RawConstantExpr::Literal(Literal::Scalar(
                        ScalarValue::from_uint(
                            self.ctx.translated.target_information.target_pointer_size,
                            *uint_ty,
                            1,
                        )
                        .or_else(|_| {
                            raise_error!(self.ctx, self.span, "One value out of bounds")
                        })?,
                    )),
                    ty: ty.clone(),
                };
                Ok(expr)
            }
            _ => raise_error!(
                self.ctx,
                self.span,
                "Unsupported type for one constant: {:?}",
                ty
            ),
        }
    }

    fn layout_constant(&self, value: Either<String, u64>) -> Result<Operand, Error> {
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
}

#[derive(Debug, Clone)]
enum DropCase {
    /// Drop function found - call it directly
    Direct(FunDeclRef),
    /// No drop function needed (e.g., i32) - generate empty shim
    Empty,
    /// Drop function not translated - generate panic shim
    Panic(String),
    /// Unknown due to generics - generate opaque
    Unknown(String),
    /// Array traversal drop (drop each element)
    Array {
        element_ty: Ty,
        element_drop: Box<DropCase>,
    },
    /// Tuple field-by-field drop (drop each field that needs it)
    Tuple { fields: Vec<(Ty, DropCase)> },
}

impl DropCase {
    fn simplify(self) -> Self {
        match self {
            DropCase::Direct(..) | DropCase::Empty | DropCase::Panic(_) | DropCase::Unknown(_) => {
                self
            }
            DropCase::Array {
                element_ty,
                element_drop,
            } => {
                let simplified_case = element_drop.simplify();
                match &simplified_case {
                    // Quick return for simple / emergency cases
                    DropCase::Empty | DropCase::Panic(_) | DropCase::Unknown(_) => simplified_case,
                    // Other cases, simply keep the structure
                    DropCase::Array { .. } | DropCase::Direct(_) | DropCase::Tuple { .. } => {
                        DropCase::Array {
                            element_ty,
                            element_drop: Box::new(simplified_case),
                        }
                    }
                }
            }
            DropCase::Tuple { fields } => {
                let mut new_fields = Vec::new();
                
                for (ty, drop_case) in fields {
                    let simplified_case = drop_case.simplify();
                    match &simplified_case {
                        // Early return for emergency cases
                        DropCase::Panic(_) | DropCase::Unknown(_) => return simplified_case,
                        // Keep other cases
                        _ => new_fields.push((ty, simplified_case)),
                    }
                }
                
                // Check if all fields are empty
                let has_non_empty = new_fields
                    .iter()
                    .any(|(_, drop_case)| !matches!(drop_case, DropCase::Empty));
                
                if has_non_empty {
                    DropCase::Tuple { fields: new_fields }
                } else {
                    DropCase::Empty
                }
            }
        }
    }
}

/// Vtable metadata computer that holds common state and provides methods
/// for computing size, align, and drop shim functions for vtable instances.
struct VtableMetadataComputer<'a> {
    ctx: &'a mut TransformCtx,
    impl_ref: &'a TraitImplRef,
    span: Span,
    /// The type of the drop field: `fn<'a>(self: &'a mut dyn Trait<...>)`
    drop_field_ty: Option<Ty>,
    generics: &'a GenericParams,
}

impl<'a> VtableMetadataComputer<'a> {
    fn new(
        ctx: &'a mut TransformCtx,
        impl_ref: &'a TraitImplRef,
        span: Span,
        generics: &'a GenericParams,
    ) -> Self {
        Self {
            ctx,
            impl_ref,
            span,
            drop_field_ty: None,
            generics,
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
        let constant_builder = ConstantBuilder::new(self.ctx, self.span);

        match concrete_ty.layout(&self.ctx.translated) {
            Ok(layout) => {
                fields[0] = constant_builder.layout_constant(match layout.size {
                    Some(size) => Either::Right(size),
                    None => Either::Left("Size not available".to_string()),
                })?;
                fields[1] = constant_builder.layout_constant(match layout.align {
                    Some(align) => Either::Right(align),
                    None => Either::Left("Align not available".to_string()),
                })?;
            }
            Err(reason) => {
                let reason_msg = format!("Layout not available: {}", reason);
                fields[0] = constant_builder.layout_constant(Either::Left(reason_msg.clone()))?;
                fields[1] = constant_builder.layout_constant(Either::Left(reason_msg))?;
            }
        }
        Ok(())
    }

    // ========================================
    // VTABLE DETECTION AND TYPE EXTRACTION
    // ========================================

    /// Check if a type ID represents a vtable struct
    fn is_vtable_struct(&self, type_id: &TypeId) -> Result<bool, Error> {
        let TypeId::Adt(type_decl_id) = type_id else {
            return Ok(false);
        };

        let Some(trait_impl) = self.ctx.translated.trait_impls.get(self.impl_ref.id) else {
            raise_error!(
                self.ctx,
                self.span,
                "Trait impl not found: {}",
                self.impl_ref.id.with_ctx(&self.ctx.into_fmt())
            );
        };

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

        trace!(
            "[DropShim] Generating drop shim for type: {}",
            concrete_ty.with_ctx(&self.ctx.into_fmt())
        );

        self.create_drop_shim(concrete_ty, &drop_case)
    }

    fn get_drop_field_ty(&self) -> Result<&Ty, Error> {
        match self.drop_field_ty {
            Some(ref ty) => Ok(ty),
            None => raise_error!(self.ctx, self.span, "Drop field type not initialized"),
        }
    }

    /// Create a drop shim function for the given case
    fn create_drop_shim(
        &mut self,
        concrete_ty: &Ty,
        drop_case: &DropCase,
    ) -> Result<Operand, Error> {
        // shortcut return for Unknown as opaque
        match drop_case {
            DropCase::Unknown(reason) => {
                return Ok(Operand::opaque(
                    format!("Unknown drop case: {}", reason),
                    self.get_drop_field_ty()?.clone(),
                ));
            }
            _ => {}
        }

        let shim_id = self.create_drop_shim_function(drop_case, concrete_ty)?;

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
        trace!("[ANALYZE] Analyzing drop case for type: {:?}", concrete_ty);

        match concrete_ty.kind() {
            TyKind::Adt(type_decl_ref) => self.analyze_adt_drop_case(type_decl_ref, concrete_ty),
            // Those having no drop behavior: literal have no drop
            TyKind::Literal(_) |
            // The reference and pointer types do not need drop
            TyKind::Ref(..) | TyKind::RawPtr(..) |
            // Other types that do not need drop
            TyKind::DynTrait(..) | TyKind::FnPtr(..) | TyKind::FnDef(..) => Ok(DropCase::Empty),
            // Other types with unknown drop behavior
            TyKind::TypeVar(..) | TyKind::Never | TyKind::TraitType(..) | TyKind::Error(_) => {
                Ok(DropCase::Unknown(format!(
                    "Non-concrete type for drop analysis: {}",
                    self.type_to_string(concrete_ty)
                )))
            }
        }
    }

    /// Analyze drop case for ADT types (arrays, tuples, structs, enums)
    fn analyze_adt_drop_case(
        &self,
        type_decl_ref: &TypeDeclRef,
        concrete_ty: &Ty,
    ) -> Result<DropCase, Error> {
        match &type_decl_ref.id {
            TypeId::Builtin(BuiltinTy::Array) => self.analyze_array_drop_case(type_decl_ref),
            TypeId::Tuple => self.analyze_tuple_drop_case(type_decl_ref),
            TypeId::Builtin(builtin_ty) => self.analyze_builtin_drop_case(builtin_ty, concrete_ty),
            TypeId::Adt(_) => {
                trace!("[ANALYZE] Found ADT type, looking for direct drop implementation");
                if let Some(fun_ref) = self.find_direct_drop_impl(concrete_ty)? {
                    Ok(DropCase::Direct(fun_ref))
                } else {
                    Ok(DropCase::Panic(format!(
                        "Drop implementation for {:?} not found or not translated",
                        concrete_ty
                    )))
                }
            }
        }
    }

    /// Analyze drop case for array types [T; N]
    fn analyze_array_drop_case(&self, type_decl_ref: &TypeDeclRef) -> Result<DropCase, Error> {
        let Some(element_ty) = type_decl_ref.generics.types.get(TypeVarId::new(0)) else {
            return Ok(DropCase::Unknown(
                "Array type missing element type parameter".to_string(),
            ));
        };

        if self.is_array_empty(type_decl_ref)? {
            return Ok(DropCase::Empty);
        }

        let element_case = self.analyze_drop_case(element_ty)?;
        match element_case {
            DropCase::Empty | DropCase::Panic(_) | DropCase::Unknown(_) => Ok(element_case),
            _ => Ok(DropCase::Array {
                element_ty: element_ty.clone(),
                element_drop: Box::new(element_case),
            }),
        }
    }

    /// Analyze drop case for tuple types (T1, T2, ...)
    fn analyze_tuple_drop_case(&self, type_decl_ref: &TypeDeclRef) -> Result<DropCase, Error> {
        let tuple_generics = &type_decl_ref.generics.types;
        let mut fields = Vec::new();

        for (_idx, field_ty) in tuple_generics.iter().enumerate() {
            let field_case = self.analyze_drop_case(field_ty)?;

            match field_case {
                DropCase::Panic(_) | DropCase::Unknown(_) => {
                    return Ok(field_case);
                }
                _ => {
                    fields.push((field_ty.clone(), field_case));
                }
            }
        }

        if fields.is_empty() {
            Ok(DropCase::Empty)
        } else {
            let has_drops = fields
                .iter()
                .any(|(_, drop_case)| !matches!(drop_case, DropCase::Empty));

            if !has_drops {
                Ok(DropCase::Empty)
            } else {
                Ok(DropCase::Tuple { fields })
            }
        }
    }

    /// Analyze drop case for builtin types (Box, Slice, etc.)
    fn analyze_builtin_drop_case(
        &self,
        builtin_ty: &BuiltinTy,
        concrete_ty: &Ty,
    ) -> Result<DropCase, Error> {
        match builtin_ty {
            BuiltinTy::Array => {
                unreachable!("Array should be handled separately")
            }
            BuiltinTy::Box => {
                if let Some(fun_ref) = self.find_direct_drop_impl(concrete_ty)? {
                    Ok(DropCase::Direct(fun_ref))
                } else {
                    Ok(DropCase::Panic(
                        "Box Drop implementation not found or not translated".to_string(),
                    ))
                }
            }
            BuiltinTy::Slice => Ok(DropCase::Empty),
            BuiltinTy::Str => Ok(DropCase::Empty),
        }
    }

    /// Check if an array has length 0
    fn is_array_empty(&self, type_decl_ref: &TypeDeclRef) -> Result<bool, Error> {
        if let Some(const_val) = type_decl_ref
            .generics
            .const_generics
            .get(ConstGenericVarId::new(0))
        {
            if let ConstGeneric::Value(literal) = const_val {
                if let Literal::Scalar(ScalarValue::Unsigned(_, 0))
                | Literal::Scalar(ScalarValue::Signed(_, 0)) = literal
                {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    // ========================================
    // DROP TRAIT DETECTION AND IMPLEMENTATION LOOKUP
    // ========================================

    /// Check if a trait declaration is the Drop trait
    fn is_drop_trait(&self, trait_decl_id: &TraitDeclId) -> bool {
        if let Some(trait_decl) = self.ctx.translated.trait_decls.get(*trait_decl_id) {
            if let Some(ref lang_item) = trait_decl.item_meta.lang_item {
                return lang_item == "drop";
            }
        }
        false
    }

    /// Find direct Drop implementation for a concrete type
    fn find_direct_drop_impl(&self, concrete_ty: &Ty) -> Result<Option<FunDeclRef>, Error> {
        trace!(
            "[DROP_IMPL] Looking for drop implementation for type: {:?}",
            concrete_ty
        );

        for trait_impl in self.ctx.translated.trait_impls.iter() {
            let trait_decl_id = trait_impl.impl_trait.id;

            if self.is_drop_trait(&trait_decl_id) {
                if let Some(self_type) = self.get_impl_self_type(trait_impl) {
                    trace!("[DROP_IMPL] Checking impl with self type: {:?}", self_type);

                    if self.types_match(&self_type, concrete_ty) {
                        trace!("[DROP_IMPL] Found matching drop impl");

                        // Find the drop method in this implementation
                        if let Some((method_name, method_ref)) =
                            trait_impl.methods().find(|(n, _)| n.0 == "drop")
                        {
                            trace!(
                                "[DROP_IMPL] Found drop method: {}, {:?}",
                                method_name, method_ref
                            );
                            // Extract the FunDeclRef from the binder
                            return Ok(Some(method_ref.skip_binder.clone()));
                        }

                        trace!("[DROP_IMPL] No drop method found in matching impl");
                    }
                }
            }
        }

        trace!(
            "[DROP_IMPL] No drop implementation found for type: {:?}",
            concrete_ty
        );
        Ok(None)
    }

    // ========================================
    // TYPE UTILITIES
    // ========================================

    /// Get the self type from a trait implementation
    fn get_impl_self_type(&self, trait_impl: &TraitImpl) -> Option<Ty> {
        if let Some(first_generic) = trait_impl.impl_trait.generics.types.get(TypeVarId::new(0)) {
            Some(first_generic.clone())
        } else {
            None
        }
    }

    /// Check if this is a built-in Box type (vs ADT Box)
    fn is_builtin_box(&self, ty: &Ty) -> bool {
        match ty.kind() {
            TyKind::Adt(TypeDeclRef {
                id: TypeId::Builtin(BuiltinTy::Box),
                ..
            }) => true,
            _ => false,
        }
    }

    /// Check if two types match for the purpose of drop implementation lookup
    fn types_match(&self, ty1: &Ty, ty2: &Ty) -> bool {
        match (ty1.kind(), ty2.kind()) {
            (TyKind::Adt(ref1), TyKind::Adt(ref2)) => ref1.id == ref2.id,
            (TyKind::Literal(lit1), TyKind::Literal(lit2)) => lit1 == lit2,
            _ => ty1 == ty2,
        }
    }

    /// Convert a type to a string representation for display purposes
    fn type_to_string(&self, ty: &Ty) -> String {
        ty.with_ctx(&self.ctx.into_fmt()).to_string()
    }

    // ========================================
    // NEW DROP CASE ANALYSIS METHODS
    // ========================================

    /// Find direct drop implementation for a concrete type
    // ========================================
    // DROP SHIM FUNCTION CREATION
    // ========================================

    /// Different kinds of drop shims we need to generate
    fn create_drop_shim_function(
        &mut self,
        drop_case: &DropCase,
        concrete_ty: &Ty,
    ) -> Result<FunDeclId, Error> {
        let shim_name = format!(
            "{{{}}}::{{vtable}}::{{drop_method}}",
            self.impl_ref.id.with_ctx(&self.ctx.into_fmt())
        );
        // Get the generics from the trait impl - drop shims should have the same generics
        let generics = self.get_trait_impl_generics()?;

        // Create the dyn trait type for the parameter
        // For the drop shim, we need &mut (dyn Trait<...>)
        let dyn_trait_param_ty = self.get_drop_receiver()?;

        // Create function signature with proper generics
        let signature = FunSig {
            is_unsafe: false,
            generics,
            inputs: vec![dyn_trait_param_ty.clone()],
            output: Ty::mk_unit(),
        };

        let body = DropShimCtx::create_drop_shim_body(self, &self.get_drop_receiver()?, concrete_ty, drop_case)?;

        // Create item meta
        let item_meta = ItemMeta {
            span: self.span,
            name: Name::from_path(&[&shim_name]),
            source_text: None,
            attr_info: AttrInfo::default(),
            is_local: true,
            opacity: ItemOpacity::Transparent, // Mark as transparent so the name shows up
            lang_item: None,
        };
        let shim_name = item_meta.name.clone();

        // Create and add function declaration
        let shim_id = self.ctx.translated.fun_decls.push_with(|id| FunDecl {
            def_id: id,
            item_meta,
            signature,
            kind: ItemKind::TopLevel,
            is_global_initializer: None,
            body: Ok(body),
        });
        self.ctx
            .translated
            .item_names
            .insert(AnyTransId::Fun(shim_id), shim_name);

        Ok(shim_id)
    }

    fn old_create_drop_shim_body(&mut self, dyn_trait_param_ty: &Ty, concrete_ty: &Ty, drop_case: &DropCase) -> Result<Body, Error> {
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

        trace!(
            "Generating drop shim for type: {}",
            concrete_ty.with_ctx(&self.ctx.into_fmt())
        );

        // Create function body based on the drop case
        let body = match drop_case {
            DropCase::Direct(fun_ref) => {
                trace!("Generating direct drop shim call.");
                // Generate a call to the drop function with Concretize cast
                self.create_direct_drop_body(
                    &mut locals,
                    &dyn_trait_param_ty,
                    concrete_ty,
                    fun_ref,
                )?
            }
            DropCase::Empty => {
                trace!("Generating empty drop shim.");
                // Generate an empty function that just returns
                self.create_empty_drop_body(&locals)?
            }
            DropCase::Panic(msg) => {
                trace!("Generating panic drop shim.");
                // Generate a function that panics
                self.create_panic_drop_body(&locals, &msg)?
            }
            DropCase::Array {
                element_ty,
                element_drop,
            } => {
                trace!("Generating array drop shim.");
                // Generate array traversal drop logic placeholder
                self.create_array_drop_body(
                    &mut locals,
                    &dyn_trait_param_ty,
                    concrete_ty,
                    element_ty,
                    element_drop,
                )?
            }
            DropCase::Tuple { fields } => {
                trace!("Generating tuple drop shim.");
                // Generate tuple field-by-field drop logic placeholder
                self.create_tuple_drop_body(&mut locals, &dyn_trait_param_ty, concrete_ty, fields)?
            }
            DropCase::Unknown(_) => {
                unreachable!()
            }
        };

        Ok(body)
    }

    /// Create a drop function call statement and terminator - abstracted from direct drop logic
    fn create_drop_call(
        &self,
        locals: &Locals,
        fun_ref: &FunDeclRef,
        operand: Operand,
        target_block: BlockId,
        concrete_ty: &Ty,
    ) -> Result<Terminator, Error> {
        let call = Call {
            func: FnOperand::Regular(FnPtr::from(FunDeclRef {
                id: fun_ref.id,
                generics: self.create_drop_function_generics(concrete_ty)?,
            })),
            args: vec![operand],
            dest: locals.return_place(),
        };

        Ok(Terminator {
            span: self.span,
            content: RawTerminator::Call {
                call,
                target: target_block,
                on_unwind: target_block, // Simple unwind handling
            },
            comments_before: vec!["Call drop function".to_string()],
        })
    }

    /// Create function body for direct drop calls
    fn create_direct_drop_body(
        &self,
        locals: &mut Locals,
        dyn_trait_param_ty: &Ty,
        concrete_ty: &Ty,
        fun_ref: &FunDeclRef,
    ) -> Result<Body, Error> {
        let mut blocks = Vector::new();

        // Create the concrete type (mutable reference to the concrete type)
        let concrete_ref_ty =
            TyKind::Ref(Region::Erased, concrete_ty.clone(), RefKind::Mut).into_ty();

        let concrete_local = Local {
            index: LocalId::new(2),
            name: Some("concrete".to_string()),
            ty: concrete_ref_ty.clone(),
        };

        let concrete_local_id = locals.locals.push_with(|_| concrete_local);

        // Create the concretize cast statement
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
                id: fun_ref.id,
                generics: self.create_drop_function_generics(concrete_ty)?,
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
            comments_before: vec!["Call drop function on concretized value".to_string()],
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

        Ok(Body::Unstructured(ExprBody {
            span: self.span,
            locals: locals.clone(),
            comments: vec![],
            body: blocks,
        }))
    }

    /// Create function body for empty drop functions
    fn create_empty_drop_body(&self, locals: &Locals) -> Result<Body, Error> {
        let mut blocks = Vector::new();
        let _ = blocks.push_with(|_| BlockData {
            statements: vec![],
            terminator: Terminator {
                span: self.span,
                content: RawTerminator::Return,
                comments_before: vec![],
            },
        });

        Ok(Body::Unstructured(ExprBody {
            span: self.span,
            locals: locals.clone(),
            comments: vec![],
            body: blocks,
        }))
    }

    /// Create function body for panic drop functions
    fn create_panic_drop_body(&self, locals: &Locals, msg: &str) -> Result<Body, Error> {
        let mut blocks = Vector::new();
        let _ = blocks.push_with(|_| BlockData {
            statements: vec![],
            terminator: Terminator {
                span: self.span,
                content: RawTerminator::Abort(AbortKind::Panic(None)),
                comments_before: vec![format!("Panic: {}", msg)],
            },
        });

        Ok(Body::Unstructured(ExprBody {
            span: self.span,
            locals: locals.clone(),
            comments: vec![],
            body: blocks,
        }))
    }

    /// Get array length from the concrete type (for [T; N], N is the const generic)
    fn get_array_length(&self, concrete_ty: &Ty) -> Result<ConstantExpr, Error> {
        if let TyKind::Adt(type_decl_ref) = concrete_ty.kind() {
            if let Some(const_val) = type_decl_ref
                .generics
                .const_generics
                .get(ConstGenericVarId::new(0))
            {
                match const_val {
                    ConstGeneric::Global(global_decl_id) => {
                        let ty = self
                            .ctx
                            .translated
                            .global_decls
                            .get(*global_decl_id)
                            .unwrap()
                            .ty
                            .clone();
                        Ok(ConstantExpr {
                            value: RawConstantExpr::Global(GlobalDeclRef {
                                id: *global_decl_id,
                                generics: Box::new(GenericArgs::empty()),
                            }),
                            ty,
                        })
                    }
                    ConstGeneric::Var(de_bruijn_var) => {
                        let DeBruijnVar::Bound(_, var_id) = de_bruijn_var else {
                            unreachable!()
                        };
                        let ty = self
                            .generics
                            .const_generics
                            .get(*var_id)
                            .unwrap()
                            .ty
                            .clone()
                            .into();
                        Ok(ConstantExpr {
                            value: RawConstantExpr::Var(de_bruijn_var.clone()),
                            ty,
                        })
                    }
                    ConstGeneric::Value(literal) => Ok(literal.clone().into()),
                }
            } else {
                raise_error!(
                    self.ctx,
                    self.span,
                    "Array type missing length const generic"
                )
            }
        } else {
            raise_error!(
                self.ctx,
                self.span,
                "Expected array type, found: {:?}",
                concrete_ty
            )
        }
    }

    /// Create a constant expression with value 0 of the same type as the array length
    fn create_array_drop_body(
        &self,
        locals: &mut Locals,
        dyn_trait_param_ty: &Ty,
        concrete_ty: &Ty,
        element_ty: &Ty,
        element_drop: &DropCase,
    ) -> Result<Body, Error> {
        let mut blocks = Vector::new();
        let constant_builder = ConstantBuilder::new(self.ctx, self.span);

        // Get array length as a constant expression
        let array_length_expr = self.get_array_length(concrete_ty)?;

        // Create a concretize cast from dyn trait to concrete array type
        let concrete_array_ref_ty =
            TyKind::Ref(Region::Erased, concrete_ty.clone(), RefKind::Mut).into_ty();

        // Add local for the concretized array using the utility method
        let concrete = locals.new_var(Some("concrete_array".into()), concrete_array_ref_ty.clone());

        // Create the receiver place from the self parameter
        let receiver = locals.place_for_var(LocalId::new(1));

        // Create the concretize cast statement
        let concretize_stmt = Statement {
            span: self.span,
            content: RawStatement::Assign(
                concrete.clone(),
                Rvalue::UnaryOp(
                    UnOp::Cast(CastKind::Concretize(
                        dyn_trait_param_ty.clone(),
                        concrete_array_ref_ty.clone(),
                    )),
                    Operand::Move(receiver),
                ),
            ),
            comments_before: vec![format!(
                "Concretize to concrete array type - element type: {:?}",
                element_ty
            )],
        };

        match element_drop {
            DropCase::Direct(fun_ref) => {
                // Elements need direct drop function calls
                // Create loop structure to drop each element

                // Get the type from array_length_expr for counter
                let counter_ty = &array_length_expr.ty;

                // Add counter local for loop iteration using utility method
                let counter = locals.new_var(Some("counter".to_string()), counter_ty.clone());

                // Create zero constant with the same type as array length
                let zero_constant = constant_builder.zero_constant(counter_ty)?;

                // Block 0: Setup - concretize and initialize counter
                let counter_init_stmt = Statement {
                    span: self.span,
                    content: RawStatement::Assign(
                        counter.clone(),
                        Rvalue::Use(Operand::Const(Box::new(zero_constant))),
                    ),
                    comments_before: vec!["Initialize counter to 0".to_string()],
                };

                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![concretize_stmt, counter_init_stmt],
                    terminator: Terminator::goto(self.span, BlockId::new(1)), // Go to loop condition
                });

                // Block 1: Loop condition check
                let counter_check = locals.new_var(
                    Some("counter_check".to_string()),
                    TyKind::Literal(LiteralTy::Bool).into_ty(),
                );

                let counter_check_stmt = Statement {
                    span: self.span,
                    content: RawStatement::Assign(
                        counter_check.clone(),
                        Rvalue::BinaryOp(
                            BinOp::Lt,
                            Operand::Copy(counter.clone()),
                            Operand::Const(Box::new(array_length_expr.clone())),
                        ),
                    ),
                    comments_before: vec![format!("Check if counter < array length")],
                };

                let loop_switch = SwitchTargets::If(
                    BlockId::new(2), // true: go to loop body
                    BlockId::new(4), // false: go to return
                );

                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![counter_check_stmt],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Switch {
                            discr: Operand::Move(counter_check.clone()),
                            targets: loop_switch,
                        },
                        comments_before: vec!["Branch based on loop condition".to_string()],
                    },
                });

                // Block 2: Loop body - drop current element
                let element_ref_ty =
                    TyKind::Ref(Region::Erased, element_ty.clone(), RefKind::Mut).into_ty();

                let element_ref =
                    locals.new_var(Some("element_ref".to_string()), element_ref_ty.clone());

                // Create array index projection - need to dereference the reference first
                let index_projection = ProjectionElem::Index {
                    offset: Box::new(Operand::Copy(counter.clone())),
                    from_end: false,
                };

                // Dereference the concrete reference to get the array, then index
                let deref_place = concrete
                    .clone()
                    .project(ProjectionElem::Deref, concrete_ty.clone());
                let projected_place = deref_place.project(index_projection, element_ref_ty.clone());

                let element_proj_stmt = Statement {
                    span: self.span,
                    content: RawStatement::Assign(
                        element_ref.clone(),
                        Rvalue::Ref(projected_place, BorrowKind::Mut),
                    ),
                    comments_before: vec!["Project to current array element".to_string()],
                };

                // Create drop function call using the abstracted helper
                let drop_terminator = self.create_drop_call(
                    locals,
                    fun_ref,
                    Operand::Move(element_ref.clone()),
                    BlockId::new(3), // Go to counter increment
                    element_ty,      // Pass element type for generics
                )?;

                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![element_proj_stmt],
                    terminator: drop_terminator,
                });

                // Block 3: Increment counter and loop back
                let one_constant = constant_builder.one_constant(counter_ty)?;

                let counter_increment_stmt = Statement {
                    span: self.span,
                    content: RawStatement::Assign(
                        counter.clone(),
                        Rvalue::BinaryOp(
                            BinOp::Add(OverflowMode::Panic),
                            Operand::Move(counter.clone()),
                            Operand::Const(Box::new(one_constant)),
                        ),
                    ),
                    comments_before: vec!["Increment counter".to_string()],
                };

                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![counter_increment_stmt],
                    terminator: Terminator::goto(self.span, BlockId::new(1)), // Go back to condition
                });

                // Block 4: Return block
                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec!["Array drop complete".to_string()],
                    },
                });
            }
            DropCase::Panic(msg) => {
                // Element drop is not translated - create panic
                let panic_stmt = Statement {
                    span: self.span,
                    content: RawStatement::Assign(
                        locals.return_place(),
                        Rvalue::Use(Operand::opaque(
                            format!("panic: array element drop not translated: {}", msg),
                            Ty::mk_unit(),
                        )),
                    ),
                    comments_before: vec![
                        "Panic for array element drop not translated".to_string(),
                    ],
                };

                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![concretize_stmt, panic_stmt],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec![],
                    },
                });
            }
            _ => {
                // Other cases (including nested arrays/tuples) - handle recursively
                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![concretize_stmt],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec![
                            "Array drop - nested case not yet implemented".to_string(),
                        ],
                    },
                });
            }
        }

        Ok(Body::Unstructured(ExprBody {
            span: self.span,
            locals: locals.clone(),
            comments: vec![],
            body: blocks,
        }))
    }

    /// Create function body for tuple drop functions - field-by-field drop
    fn create_tuple_drop_body(
        &self,
        locals: &mut Locals,
        dyn_trait_param_ty: &Ty,
        concrete_ty: &Ty,
        fields: &[(Ty, DropCase)],
    ) -> Result<Body, Error> {
        let mut blocks = Vector::new();

        // Create a concretize cast from dyn trait to concrete tuple type
        let concrete_tuple_ref_ty =
            TyKind::Ref(Region::Erased, concrete_ty.clone(), RefKind::Mut).into_ty();

        // Add local for the concretized tuple using utility method
        let concrete = locals.new_var(
            Some("concrete_tuple".to_string()),
            concrete_tuple_ref_ty.clone(),
        );

        // Create the receiver place from the self parameter
        let receiver = locals.place_for_var(LocalId::new(1));

        // Create the concretize cast statement
        let concretize_stmt = Statement {
            span: self.span,
            content: RawStatement::Assign(
                concrete.clone(),
                Rvalue::UnaryOp(
                    UnOp::Cast(CastKind::Concretize(
                        dyn_trait_param_ty.clone(),
                        concrete_tuple_ref_ty.clone(),
                    )),
                    Operand::Move(receiver),
                ),
            ),
            comments_before: vec![format!(
                "Concretize to concrete tuple type - {} fields",
                fields.len()
            )],
        };

        if fields.is_empty() {
            // Empty tuple - just return immediately
            let _ = blocks.push_with(|_| BlockData {
                statements: vec![],
                terminator: Terminator {
                    span: self.span,
                    content: RawTerminator::Return,
                    comments_before: vec!["Empty tuple, nothing to drop".to_string()],
                },
            });
        } else {
            // Filter out fields that don't need dropping to avoid unnecessary field access
            let fields_needing_drop: Vec<_> = fields
                .iter()
                .enumerate()
                .filter(|(_, (_, drop_case))| !matches!(drop_case, DropCase::Empty))
                .collect();

            // First block: concretize
            let _ = blocks.push_with(|_| BlockData {
                statements: vec![concretize_stmt],
                terminator: if fields_needing_drop.is_empty() {
                    // No fields need dropping - go directly to return
                    Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec!["No tuple fields need dropping".to_string()],
                    }
                } else {
                    // Go to field processing
                    Terminator::goto(self.span, BlockId::new(1))
                },
            });

            if !fields_needing_drop.is_empty() {
                let mut block_id = BlockId::new(1);

                // Create drop blocks only for fields that need dropping
                for (idx, (field_idx, (field_ty, drop_case))) in
                    fields_needing_drop.iter().enumerate()
                {
                    let is_last_field = idx == fields_needing_drop.len() - 1;
                    let next_block_id = if is_last_field {
                        BlockId::new(block_id.index() + 1) // Return block
                    } else {
                        BlockId::new(block_id.index() + 1) // Next field block
                    };

                    match drop_case {
                        DropCase::Direct(fun_ref) => {
                            // Create field reference with proper projection
                            let field_projection = ProjectionElem::Field(
                                FieldProjKind::Tuple(fields.len()),
                                FieldId::new(*field_idx),
                            );

                            let field_ref_ty =
                                TyKind::Ref(Region::Erased, field_ty.clone(), RefKind::Mut)
                                    .into_ty();

                            // Add local for field reference using utility method
                            let field_ref = locals.new_var(
                                Some(format!("field_{}_ref", field_idx)),
                                field_ref_ty.clone(),
                            );

                            // Create field projection statement - need to dereference the reference first
                            let deref_place = concrete
                                .clone()
                                .project(ProjectionElem::Deref, concrete_ty.clone());
                            let projected_place =
                                deref_place.project(field_projection, field_ref_ty.clone());

                            let field_assign_stmt = Statement {
                                span: self.span,
                                content: RawStatement::Assign(
                                    field_ref.clone(),
                                    Rvalue::Ref(projected_place, BorrowKind::Mut),
                                ),
                                comments_before: vec![format!(
                                    "Get reference to tuple field {}",
                                    field_idx
                                )],
                            };

                            // Create drop call statement using the abstracted helper
                            let drop_terminator = self.create_drop_call(
                                locals,
                                fun_ref,
                                Operand::Move(field_ref),
                                next_block_id,
                                field_ty, // Pass field type for generics
                            )?;

                            let _ = blocks.push_with(|block_id| {
                                let _ = block_id;
                                BlockData {
                                    statements: vec![field_assign_stmt],
                                    terminator: drop_terminator,
                                }
                            });
                        }
                        DropCase::Panic(msg) => {
                            // Field drop is not translated - create panic
                            let panic_stmt = Statement {
                                span: self.span,
                                content: RawStatement::Assign(
                                    locals.return_place(),
                                    Rvalue::Use(Operand::opaque(
                                        format!(
                                            "panic: tuple field {} drop not translated: {}",
                                            field_idx, msg
                                        ),
                                        Ty::mk_unit(),
                                    )),
                                ),
                                comments_before: vec![format!(
                                    "Panic for tuple field {} drop not translated",
                                    field_idx
                                )],
                            };

                            let terminator = Terminator::goto(self.span, next_block_id);

                            let _ = blocks.push_with(|block_id| {
                                let _ = block_id;
                                BlockData {
                                    statements: vec![panic_stmt],
                                    terminator,
                                }
                            });
                        }
                        _ => {
                            // Other cases - not implemented yet
                            let comment_stmt = Statement {
                                span: self.span,
                                content: RawStatement::Assign(
                                    locals.return_place(),
                                    Rvalue::Use(Operand::opaque(
                                        format!(
                                            "nested drop not implemented for field {}",
                                            field_idx
                                        ),
                                        Ty::mk_unit(),
                                    )),
                                ),
                                comments_before: vec![format!(
                                    "Nested drop case for tuple field {} not implemented",
                                    field_idx
                                )],
                            };

                            let terminator = Terminator::goto(self.span, next_block_id);

                            let _ = blocks.push_with(|block_id| {
                                let _ = block_id;
                                BlockData {
                                    statements: vec![comment_stmt],
                                    terminator,
                                }
                            });
                        }
                    }

                    block_id = next_block_id;
                }

                // Return block
                let _ = blocks.push_with(|_| BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: self.span,
                        content: RawTerminator::Return,
                        comments_before: vec!["Tuple drop complete".to_string()],
                    },
                });
            }
        }

        Ok(Body::Unstructured(ExprBody {
            span: self.span,
            locals: locals.clone(),
            comments: vec![],
            body: blocks,
        }))
    }
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
        // but with at least one region binder for the receiver
        let mut generics = trait_impl.generics.clone();

        // Ensure we have at least one region for the receiver parameter
        if generics.regions.is_empty() {
            let _ = generics.regions.push_with(|id| RegionVar {
                index: id,
                name: None,
            });
        }

        Ok(generics)
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

    /// Get the translated `Global` type, which should always be present as `Box` is translated
    fn get_global_allocator_ty(&self) -> Result<Ty, Error> {
        for ty_decl in &self.ctx.translated.type_decls {
            if ty_decl.item_meta.lang_item == Some("global_alloc_ty".into()) {
                return Ok(Ty::new(TyKind::Adt(TypeDeclRef {
                    id: ty_decl.def_id.into(),
                    generics: Box::new(GenericArgs::empty()),
                })));
            }
        }
        raise_error!(self.ctx, self.span, "Global allocator type not found")
    }

    /// Generic args for the drop call: reuse the concrete type's generics.
    /// For `impl<Args> Drop for T<Args'>` Rustc ensures Args == Args' match, so we just forward them.
    /// Additionally, we should add the lifetime as the first region argument for the `&mut self` receiver.
    fn create_drop_function_generics(&self, concrete_ty: &Ty) -> Result<Box<GenericArgs>, Error> {
        let mut generics = match concrete_ty.kind() {
            TyKind::Adt(type_decl_ref) => {
                let mut generic_args = type_decl_ref.generics.clone();

                // Special handling for Box types
                if self.is_builtin_box(concrete_ty) {
                    generic_args.types.push(self.get_global_allocator_ty()?);
                }

                generic_args
            }
            _ => {
                raise_error!(
                    self.ctx,
                    self.span,
                    "Expected ADT type as concrete type for drop function generics, found: {:?}",
                    concrete_ty
                );
            }
        };

        // Use proper region variable instead of Region::Erased
        let region_var = Region::Var(DeBruijnVar::bound(DeBruijnId::new(0), RegionId::new(0)));
        generics
            .regions
            .insert_and_shift_ids(RegionId::ZERO, region_var);
        Ok(generics)
    }

    /// Create the generic arguments for referencing the drop shim function itself
    /// This should include all the generics that the drop shim function was defined with
    fn create_drop_shim_function_generics(&self) -> Result<GenericArgs, Error> {
        // The drop shim function reference should use the same generic arguments as the impl_ref
        // but also include the region parameter for the receiver
        let mut generics = *self.impl_ref.generics.clone();

        // We create the function pointer there, which is `fn<'a>(&'a mut dyn Trait<...>)`
        // But the shim itself should have erased region for this
        generics
            .regions
            .insert_and_shift_ids(RegionId::ZERO, Region::Erased);

        Ok(generics)
    }
}

struct DropShimCtx<'a> {
    ctx: &'a VtableMetadataComputer<'a>,
    locals: &'a mut Locals,
    blocks: Vector<BlockId, BlockData>,
    unwind_block: Option<BlockId>,
}

impl<'a> IntoFormatter for &'_ DropShimCtx<'a> {
    type C = <&'a TransformCtx as IntoFormatter>::C;
    fn into_fmt(self) -> Self::C {
        self.ctx.ctx.into_fmt()
    }
}

impl<'a> DropShimCtx<'a> {
    /// Create a new context with the given initial concretize statement in the initial block
    fn new(ctx: &'a VtableMetadataComputer, locals: &'a mut Locals) -> Self {
        let mut ret = Self {
            ctx,
            locals,
            blocks: Vector::new(),
            unwind_block: None,
        };
        // create the new initial block, which must exist
        let _ = ret.new_block();
        ret
    }

    /// Get the initial block, i.e., block 0, which always exists
    fn init_block(&mut self) -> &mut BlockData {
        self.blocks.get_mut(BlockId::new(0)).unwrap()
    }

    /// The return block
    fn func_ret_block(&mut self) -> BlockId {
        if self.blocks.elem_count() > 1 {
            BlockId::new(1)
        } else {
            let (block_id, _) = self.new_block();
            block_id
        }
    }

    fn ret_place(&self) -> Place {
        self.locals.return_place()
    }

    fn span(&self) -> Span {
        self.ctx.span
    }

    fn get_unwind_block(&mut self) -> BlockId {
        if let Some(block_id) = self.unwind_block {
            block_id
        } else {
            let span = self.span();
            let block_id = self.blocks.push_with(|_| BlockData {
                statements: vec![],
                terminator: Terminator {
                    span,
                    content: RawTerminator::Abort(AbortKind::UnwindTerminate),
                    comments_before: vec![],
                },
            });
            self.unwind_block = Some(block_id);
            block_id
        }
    }

    fn new_block(&mut self) -> (BlockId, &mut BlockData) {
        self.new_block_with_terminator(Terminator {
            span: self.span(),
            content: RawTerminator::Return,
            comments_before: vec![],
        })
    }

    fn new_block_with_terminator(&mut self, terminator: Terminator) -> (BlockId, &mut BlockData) {
        let block_id = self.blocks.push_with(|_| BlockData {
            statements: vec![],
            terminator,
        });
        let block_data = self.blocks.get_mut(block_id).unwrap();
        (block_id, block_data)
    }

    fn empty_drop_block(&mut self, end_block: BlockId) -> Result<BlockId, Error> {
        let (empty_block_id, _) =
            self.new_block_with_terminator(Terminator::goto(self.span(), end_block));
        Ok(empty_block_id)
    }

    fn panic_drop_block(&mut self, msg: &String) -> Result<BlockId, Error> {
        let (block_id, _) = self.new_block_with_terminator(Terminator {
            span: self.span(),
            content: RawTerminator::Abort(AbortKind::Panic(None)),
            comments_before: vec![format!("Panic: {}", msg)],
        });
        Ok(block_id)
    }

    /// Set the initial block's terminator to a goto to the specified target
    fn set_init_block_goto(&mut self, target: BlockId) {
        self.init_block().terminator = Terminator::goto(self.span(), target);
    }

    /// Create a new call block to call the drop function
    /// If the call succeeds, goes to `end_block`, otherwise to the unwind block
    fn direct_drop_block(
        &mut self,
        drop_place: Place,
        end_block: BlockId,
        fun_ref: &FunDeclRef,
    ) -> Result<BlockId, Error> {
        // Create a new variable with var := &mut drop_place
        // This is the argument to the drop function
        let drop_place_ref = self.new_var(None, Ty::new(TyKind::Ref(
            Region::Erased,
            drop_place.ty().clone(),
            RefKind::Mut,
        )));
        let assn_stmt = Statement {
            span: self.span(),
            content: RawStatement::Assign(
                drop_place_ref.clone(),
                Rvalue::Ref(drop_place.clone(), BorrowKind::Mut),
            ),
            comments_before: vec!["Create reference for drop argument".to_string()],
        };

        // Create the call
        let call = Call {
            func: FnOperand::Regular(FnPtr::from(FunDeclRef {
                id: fun_ref.id,
                generics: self.ctx.create_drop_function_generics(drop_place.ty())?,
            })),
            args: vec![Operand::Move(drop_place_ref)],
            dest: self.ret_place(),
        };

        // Create the terminator
        let unwind_block = self.get_unwind_block();
        let terminator = Terminator {
            span: self.span(),
            content: RawTerminator::Call {
                call,
                target: end_block,
                on_unwind: unwind_block,
            },
            comments_before: vec![format!(
                "Call drop function: {}",
                fun_ref.id.with_ctx(&self.ctx.ctx.into_fmt())
            )],
        };

        let (block_id, block_data) = self.new_block_with_terminator(terminator);

        // Add the assignment statement at the start of the block
        block_data.statements.push(assn_stmt);

        Ok(block_id)
    }

    fn new_var(&mut self, name: Option<String>, ty: Ty) -> Place {
        self.locals.new_var(name, ty)
    }

    fn get_block(&mut self, block_id: BlockId) -> &mut BlockData {
        self.blocks.get_mut(block_id).unwrap()
    }

    /// Create blocks for dropping each element of the array in a loop
    fn array_drop_blocks(
        &mut self,
        drop_place: Place,
        end_block: BlockId,
        element_ty: &Ty,
        element_drop: &DropCase,
    ) -> Result<BlockId, Error> {
        // The constant builder for creating zero and one constants
        let constant_builder = ConstantBuilder::new(self.ctx.ctx, self.span());
        // The array length constant (as expression, but should be treated as constant)
        let array_length_expr = self.ctx.get_array_length(drop_place.ty())?;

        // The blocks of this looping structure
        // The block to setup the counter
        let (setup_block, _) = self.new_block();
        // The condition check block to test if counter < array length
        let (cond_block, _) = self.new_block();
        // The loop body init block to increase counter
        let (counter_incr_block, _) = self.new_block();
        // After increasing counter, get the actual drop by recursion, which has `end_block` as the `cond_block`

        // The initial block: create counter and initialize to 0
        let counter_ty = array_length_expr.ty.clone();
        let counter = self.new_var(None, counter_ty.clone());

        // Furnish the setup block
        {
            let init_counter_stmt = Statement {
                span: self.span(),
                content: RawStatement::Assign(
                    counter.clone(),
                    Rvalue::Use(Operand::Const(Box::new(
                        constant_builder.zero_constant(&counter_ty)?,
                    ))),
                ),
                comments_before: vec!["Initialize counter to 0".to_string()],
            };

            let span = self.span();
            let setup_block_data = self.get_block(setup_block);

            setup_block_data.statements.push(init_counter_stmt);
            setup_block_data.terminator = Terminator::goto(span, cond_block);
        }

        // Furnish the condition block
        {
            let counter_check = self.new_var(None, TyKind::Literal(LiteralTy::Bool).into_ty());

            let counter_check_stmt = Statement {
                span: self.span(),
                content: RawStatement::Assign(
                    counter_check.clone(),
                    Rvalue::BinaryOp(
                        BinOp::Lt,
                        Operand::Copy(counter.clone()),
                        Operand::Const(Box::new(array_length_expr.clone())),
                    ),
                ),
                comments_before: vec![format!("Check if counter < array length")],
            };

            let loop_switch = SwitchTargets::If(
                counter_incr_block, // true: go to loop body
                end_block,          // false: go to return
            );

            let span = self.span();
            let cond_block_data = self.get_block(cond_block);

            cond_block_data.statements.push(counter_check_stmt);
            cond_block_data.terminator = Terminator {
                span,
                content: RawTerminator::Switch {
                    discr: Operand::Move(counter_check.clone()),
                    targets: loop_switch,
                },
                comments_before: vec!["Branch based on loop condition".to_string()],
            };
        }

        // Build the actual block for droping the internal
        let new_drop_place = drop_place.project(
            ProjectionElem::Index {
                offset: Box::new(Operand::Copy(counter.clone())),
                from_end: false,
            },
            element_ty.clone(),
        );
        let working_block =
            self.creat_drop_case_blocks(new_drop_place, cond_block, element_drop)?;

        // Furnish the counter increment block
        {
            let one_constant = constant_builder.one_constant(&counter_ty)?;

            let counter_increment_stmt = Statement {
                span: self.span(),
                content: RawStatement::Assign(
                    counter.clone(),
                    Rvalue::BinaryOp(
                        BinOp::Add(OverflowMode::Panic),
                        Operand::Move(counter.clone()),
                        Operand::Const(Box::new(one_constant)),
                    ),
                ),
                comments_before: vec!["Increment counter".to_string()],
            };

            let span = self.span();
            let counter_incr_block_data = self.get_block(counter_incr_block);

            counter_incr_block_data
                .statements
                .push(counter_increment_stmt);
            counter_incr_block_data.terminator = Terminator::goto(span, working_block);
        }

        // The initial block is the setup block
        Ok(setup_block)
    }

    fn tuple_drop_blocks(
        &mut self,
        drop_place: Place,
        end_block: BlockId,
        fields: &Vec<(Ty, DropCase)>,
    ) -> Result<BlockId, Error> {
        // Create a series of blocks to drop each field
        // And they chain to each other, ending in end_block
        let mut current_end_block = end_block;

        for (field_id, (ty, case)) in fields.iter().enumerate().rev() {
            // Create the field projection
            let new_drop_place = drop_place.clone().project(
                ProjectionElem::Field(FieldProjKind::Tuple(fields.len()), FieldId::new(field_id)),
                ty.clone(),
            );
            let working_block =
                self.creat_drop_case_blocks(new_drop_place, current_end_block, case)?;

            current_end_block = working_block;
        }

        // If it is the same as end_block, create an empty block to maintain the guarantee
        if current_end_block == end_block {
            self.empty_drop_block(end_block)
        } else {
            Ok(current_end_block)
        }
    }

    /// It is guaranteed that the resulting block is NOT `end_block`
    /// But it is not guaranteed that the `end_block` is reachable from the resulting block, due to panics
    /// But if there is no panic, the `end_block` is always reachable
    fn creat_drop_case_blocks(
        &mut self,
        drop_place: Place,
        end_block: BlockId,
        case: &DropCase,
    ) -> Result<BlockId, Error> {
        match case {
            DropCase::Empty => self.empty_drop_block(end_block),
            DropCase::Panic(msg) => self.panic_drop_block(msg),
            DropCase::Direct(fun_ref) => self.direct_drop_block(drop_place, end_block, fun_ref),
            DropCase::Unknown(..) => unreachable!("This should be handled at first"),
            DropCase::Array {
                element_ty,
                element_drop,
            } => self.array_drop_blocks(drop_place, end_block, element_ty, element_drop),
            DropCase::Tuple { fields } => self.tuple_drop_blocks(drop_place, end_block, fields),
        }
    }

    pub fn create_drop_shim_body(
        ctx: &VtableMetadataComputer,
        dyn_trait_param_ty: &Ty,
        concrete_ty: &Ty,
        drop_case: &DropCase,
    ) -> Result<Body, Error> {
        let mut locals = Locals {
            arg_count: 1,
            locals: Vector::new(),
        };
        // create the return place
        let _ = locals.new_var(None, Ty::mk_unit());
        let dyn_receiver =
            locals.new_var(Some("dyn_receiver".to_string()), dyn_trait_param_ty.clone());

        let drop_case = drop_case.clone().simplify();

        match drop_case {
            DropCase::Empty => {
                let mut blocks = Vector::new();
                let sole_block = BlockData {
                    statements: vec![],
                    terminator: Terminator {
                        span: ctx.span,
                        content: RawTerminator::Return,
                        comments_before: vec!["Nothing to drop, return".to_string()],
                    },
                };

                let _ = blocks.push_with(|_| sole_block);
                // Nothing to drop, just return
                return Ok(Body::Unstructured(ExprBody {
                    span: ctx.span,
                    locals,
                    comments: vec![],
                    body: blocks,
                }))
            }
            _ => {
                let concrete_place = locals.new_var(Some("concrete".into()), Ty::new(TyKind::Ref(
                    Region::Erased,
                    concrete_ty.clone(),
                    RefKind::Mut,
                )));

                let concretize_stmt = Statement {
                    span: ctx.span,
                    content: RawStatement::Assign(
                        dyn_receiver.clone(),
                        Rvalue::UnaryOp(
                            UnOp::Cast(CastKind::Concretize(
                                dyn_trait_param_ty.clone(),
                                TyKind::Ref(
                                    Region::Erased,
                                    concrete_ty.clone(),
                                    RefKind::Mut,
                                )
                                .into_ty(),
                            )),
                            Operand::Move(locals.place_for_var(LocalId::new(1))),
                        ),
                    ),
                    comments_before: vec![format!(
                        "Concretize to concrete type: {}",
                        concrete_ty.with_ctx(&ctx.ctx.into_fmt())
                    )],
                };

                let mut shim_ctx = DropShimCtx::new(ctx, &mut locals);
                shim_ctx.init_block().statements.push(concretize_stmt);
                let drop_place = concrete_place.deref();
                let end_block = shim_ctx.func_ret_block();

                let next_block = shim_ctx.creat_drop_case_blocks(drop_place, end_block, &drop_case)?;
                shim_ctx.set_init_block_goto(next_block);

                let body = shim_ctx.blocks;

                Ok(Body::Unstructured(ExprBody {
                    span: ctx.span,
                    locals,
                    comments: vec![],
                    body,
                }))
            }
        }
    }
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
                    let generics = &decl.signature.generics;
                    let mut computer =
                        VtableMetadataComputer::new(ctx, impl_ref, decl.item_meta.span, generics);

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
