//! Compute metadata for vtable instances (size, align, drop).
//!
//! This pass fills the metadata fields of vtable instances with correct values instead of opaque placeholders.
//!
//! For each vtable instance initializer function, we:
//! 1. Extract the concrete type being implemented for
//! 2. Compute size & align from the type's layout information
//! 3. Find the correct drop function and generate a shim for it
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
                    match compute_vtable_metadata_for_function(
                        ctx,
                        body,
                        impl_ref,
                        decl.item_meta.span,
                    ) {
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

/// Count vtable instances for logging
fn count_vtable_instances(ctx: &TransformCtx) -> usize {
    ctx.translated
        .fun_decls
        .iter()
        .filter(|decl| matches!(decl.kind, ItemKind::VTableInstance { .. }))
        .count()
}

/// Compute vtable metadata for a specific vtable instance initializer function
fn compute_vtable_metadata_for_function(
    ctx: &mut TransformCtx,
    body: &mut Body,
    impl_ref: &TraitImplRef,
    span: Span,
) -> Result<(), Error> {
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
                    if is_vtable_struct(&vtable_ref.id) {
                        update_vtable_metadata(ctx, vtable_ref, fields, impl_ref, span)?;
                    }
                }
            }
        }
    }

    Ok(())
}

/// Check if a type ID represents a vtable struct
fn is_vtable_struct(_type_id: &TypeId) -> bool {
    match _type_id {
        TypeId::Adt(_type_decl_id) => {
            // We could check the name or other attributes here
            // For now, assume any ADT type in vtable context is a vtable
            true
        }
        _ => false,
    }
}

/// Update the vtable metadata fields (size, align, drop) with correct values
fn update_vtable_metadata(
    ctx: &mut TransformCtx,
    _vtable_ref: &TypeDeclRef,
    fields: &mut Vec<Operand>,
    impl_ref: &TraitImplRef,
    span: Span,
) -> Result<(), Error> {
    // We expect fields in order: size, align, drop, method1, method2, ..., supertrait1, ...
    if fields.len() < 3 {
        raise_error!(
            ctx,
            span,
            "Expected at least 3 fields in vtable (size, align, drop)"
        );
    }

    // Get the concrete type from the impl
    let concrete_ty = get_concrete_type_from_impl(ctx, impl_ref, span)?;

    // 1. Update size field
    fields[0] = compute_size_operand(ctx, &concrete_ty, span)?;

    // 2. Update align field
    fields[1] = compute_align_operand(ctx, &concrete_ty, span)?;

    // 3. Update drop field
    fields[2] = compute_drop_operand(ctx, &concrete_ty, span)?;

    Ok(())
}

/// Extract the concrete type being implemented for from the trait impl reference
fn get_concrete_type_from_impl(
    ctx: &TransformCtx,
    impl_ref: &TraitImplRef,
    span: Span,
) -> Result<Ty, Error> {
    let Some(trait_impl) = ctx.translated.trait_impls.get(impl_ref.id) else {
        raise_error!(
            ctx,
            span,
            "Trait impl not found: {}",
            impl_ref.id.with_ctx(&ctx.into_fmt())
        );
    };

    // Get the self type from the trait reference
    // For a trait impl like `impl Trait for ConcreteType`, we want ConcreteType
    let trait_ref = &trait_impl.impl_trait;
    let concrete_ty = &trait_ref.generics.types[0]; // First type arg is Self

    Ok(concrete_ty.clone())
}

/// Compute the size operand for the vtable
fn compute_size_operand(
    ctx: &TransformCtx,
    concrete_ty: &Ty,
    _span: Span,
) -> Result<Operand, Error> {
    match get_type_size(ctx, concrete_ty) {
        Some(size) => {
            let ptr_size = ctx.translated.target_information.target_pointer_size;
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
                value: RawConstantExpr::Opaque("size not available due to generics".to_string()),
                ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
            };
            Ok(Operand::Const(Box::new(opaque_const)))
        }
    }
}

/// Compute the align operand for the vtable
fn compute_align_operand(
    ctx: &TransformCtx,
    concrete_ty: &Ty,
    _span: Span,
) -> Result<Operand, Error> {
    match get_type_align(ctx, concrete_ty) {
        Some(align) => {
            let ptr_size = ctx.translated.target_information.target_pointer_size;
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
                value: RawConstantExpr::Opaque("align not available due to generics".to_string()),
                ty: Ty::new(TyKind::Literal(LiteralTy::UInt(UIntTy::Usize))),
            };
            Ok(Operand::Const(Box::new(opaque_const)))
        }
    }
}

/// Compute the drop operand for the vtable
fn compute_drop_operand(
    _ctx: &mut TransformCtx,
    concrete_ty: &Ty,
    _span: Span,
) -> Result<Operand, Error> {
    // For now, generate a placeholder shim function
    // TODO: Implement the three cases for drop function resolution
    let opaque_const = ConstantExpr {
        value: RawConstantExpr::Opaque("drop shim not yet implemented".to_string()),
        ty: create_drop_fn_type(concrete_ty),
    };
    Ok(Operand::Const(Box::new(opaque_const)))
}

/// Get the size of a type from its layout information
fn get_type_size(ctx: &TransformCtx, ty: &Ty) -> Option<u128> {
    match ty.kind() {
        TyKind::Adt(type_decl_ref) => {
            let TypeId::Adt(type_decl_id) = type_decl_ref.id else {
                return None;
            };
            let type_decl = ctx.translated.type_decls.get(type_decl_id)?;
            type_decl.layout.as_ref()?.size?.try_into().ok()
        }
        TyKind::Literal(lit_ty) => {
            // For literal types, we can compute size directly
            Some(match lit_ty {
                LiteralTy::Bool => 1,
                LiteralTy::Char => 4,
                LiteralTy::Int(int_ty) => get_int_size(ctx, *int_ty),
                LiteralTy::UInt(uint_ty) => get_uint_size(ctx, *uint_ty),
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
fn get_type_align(ctx: &TransformCtx, ty: &Ty) -> Option<u128> {
    match ty.kind() {
        TyKind::Adt(type_decl_ref) => {
            let TypeId::Adt(type_decl_id) = type_decl_ref.id else {
                return None;
            };
            let type_decl = ctx.translated.type_decls.get(type_decl_id)?;
            type_decl.layout.as_ref()?.align?.try_into().ok()
        }
        TyKind::Literal(lit_ty) => {
            // For literal types, alignment is typically the same as size (with some exceptions)
            Some(match lit_ty {
                LiteralTy::Bool => 1,
                LiteralTy::Char => 4,
                LiteralTy::Int(int_ty) => get_int_size(ctx, *int_ty),
                LiteralTy::UInt(uint_ty) => get_uint_size(ctx, *uint_ty),
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
fn get_int_size(ctx: &TransformCtx, int_ty: IntTy) -> u128 {
    match int_ty {
        IntTy::Isize => ctx
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
fn get_uint_size(ctx: &TransformCtx, uint_ty: UIntTy) -> u128 {
    match uint_ty {
        UIntTy::Usize => ctx
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

/// Create the function pointer type for a drop function: `fn(*mut dyn Trait)`
fn create_drop_fn_type(concrete_ty: &Ty) -> Ty {
    // TODO: Create the correct dyn trait type based on the concrete type
    // For now, use a placeholder
    let mut_ptr_ty = TyKind::RawPtr(concrete_ty.clone(), RefKind::Mut).into_ty();
    let fn_sig = RegionBinder::empty((vec![mut_ptr_ty], Ty::mk_unit()));
    TyKind::FnPtr(fn_sig).into_ty()
}
