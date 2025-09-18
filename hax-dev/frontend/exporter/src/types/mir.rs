//! Copies of the relevant `MIR` types. MIR represents a rust (function) body as a CFG. It's a
//! semantically rich representation that contains no high-level control-flow operations like loops
//! or patterns; instead the control flow is entirely described by gotos and switches on integer
//! values.
use crate::prelude::*;
#[cfg(feature = "rustc")]
use rustc_middle::{mir, ty};

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: rustc_middle::mir::SourceInfo, state: S as s)]
pub struct SourceInfo {
    pub span: Span,
    pub scope: SourceScope,
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: rustc_middle::mir::LocalDecl<'tcx>, state: S as s)]
pub struct LocalDecl {
    pub mutability: Mutability,
    pub ty: Ty,
    pub source_info: SourceInfo,
    #[value(None)]
    pub name: Option<String>, // This information is contextual, thus the SInto instance initializes it to None, and then we fill it while `SInto`ing MirBody
}

pub type BasicBlocks = IndexVec<BasicBlock, BasicBlockData>;

#[cfg(feature = "rustc")]
fn name_of_local(
    local: rustc_middle::mir::Local,
    var_debug_info: &Vec<mir::VarDebugInfo>,
) -> Option<String> {
    var_debug_info
        .iter()
        .find(|info| {
            if let mir::VarDebugInfoContents::Place(place) = info.value {
                place.projection.is_empty() && place.local == local
            } else {
                false
            }
        })
        .map(|dbg| dbg.name.to_ident_string())
}

/// Enumerates the kinds of Mir bodies. TODO: use const generics
/// instead of an open list of types.
pub mod mir_kinds {
    use crate::prelude::{JsonSchema, derive_group};

    #[derive_group(Serializers)]
    #[derive(Clone, Copy, Debug, JsonSchema)]
    pub struct Built;

    #[derive_group(Serializers)]
    #[derive(Clone, Copy, Debug, JsonSchema)]
    pub struct Promoted;

    #[derive_group(Serializers)]
    #[derive(Clone, Copy, Debug, JsonSchema)]
    pub struct Elaborated;

    #[derive_group(Serializers)]
    #[derive(Clone, Copy, Debug, JsonSchema)]
    pub struct Optimized;

    #[derive_group(Serializers)]
    #[derive(Clone, Copy, Debug, JsonSchema)]
    pub struct CTFE;

    /// MIR of unknown origin. `body()` returns `None`; this is used to get the bodies provided via
    /// `from_mir` but not attempt to get MIR for functions etc.
    #[derive_group(Serializers)]
    #[derive(Clone, Copy, Debug, JsonSchema)]
    pub struct Unknown;

    #[cfg(feature = "rustc")]
    pub use rustc::*;
    #[cfg(feature = "rustc")]
    mod rustc {
        use super::*;
        use rustc_middle::mir::Body;
        use rustc_middle::ty::TyCtxt;
        use rustc_span::def_id::DefId;

        pub trait IsMirKind: Clone + std::fmt::Debug {
            // CPS to deal with stealable bodies cleanly.
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T>;
        }

        impl IsMirKind for Built {
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                let id = id.as_local()?;
                let steal = tcx.mir_built(id);
                if steal.is_stolen() {
                    None
                } else {
                    Some(f(&steal.borrow()))
                }
            }
        }

        impl IsMirKind for Promoted {
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                let id = id.as_local()?;
                let (steal, _) = tcx.mir_promoted(id);
                if steal.is_stolen() {
                    None
                } else {
                    Some(f(&steal.borrow()))
                }
            }
        }

        impl IsMirKind for Elaborated {
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                let id = id.as_local()?;
                let steal = tcx.mir_drops_elaborated_and_const_checked(id);
                if steal.is_stolen() {
                    None
                } else {
                    Some(f(&steal.borrow()))
                }
            }
        }

        impl IsMirKind for Optimized {
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                Some(f(tcx.optimized_mir(id)))
            }
        }

        impl IsMirKind for CTFE {
            fn get_mir<'tcx, T>(
                tcx: TyCtxt<'tcx>,
                id: DefId,
                f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                Some(f(tcx.mir_for_ctfe(id)))
            }
        }

        impl IsMirKind for Unknown {
            fn get_mir<'tcx, T>(
                _tcx: TyCtxt<'tcx>,
                _id: DefId,
                _f: impl FnOnce(&Body<'tcx>) -> T,
            ) -> Option<T> {
                None
            }
        }
    }
}

#[cfg(feature = "rustc")]
pub use mir_kinds::IsMirKind;

/// The contents of `Operand::Const`.
#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub struct ConstOperand {
    pub span: Span,
    pub ty: Ty,
    pub kind: ConstOperandKind,
}

#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub enum ConstOperandKind {
    /// An evaluated constant represented as an expression.
    Value(ConstantExpr),
    /// Part of a MIR body that was promoted to be a constant. May not be evaluatable because of
    /// generics.
    /// It's a reference to the `DefId` of the constant. Note that rustc does not give a `DefId` to
    /// promoted constants, but we do in hax.
    Promoted(ItemRef),
}

#[cfg(feature = "rustc")]
impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, ConstOperand> for mir::ConstOperand<'tcx> {
    fn sinto(&self, s: &S) -> ConstOperand {
        let kind = translate_mir_const(s, self.span, self.const_);
        ConstOperand {
            span: self.span.sinto(s),
            ty: self.const_.ty().sinto(s),
            kind,
        }
    }
}

/// Retrieve the MIR for a promoted body.
#[cfg(feature = "rustc")]
pub fn get_promoted_mir<'tcx>(
    tcx: ty::TyCtxt<'tcx>,
    def_id: RDefId,
    promoted_id: mir::Promoted,
) -> mir::Body<'tcx> {
    if let Some(local_def_id) = def_id.as_local() {
        let (_, promoteds) = tcx.mir_promoted(local_def_id);
        if !promoteds.is_stolen() {
            promoteds.borrow()[promoted_id].clone()
        } else {
            tcx.promoted_mir(def_id)[promoted_id].clone()
        }
    } else {
        tcx.promoted_mir(def_id)[promoted_id].clone()
    }
}

#[cfg(feature = "rustc")]
/// Translate a MIR constant.
fn translate_mir_const<'tcx, S: UnderOwnerState<'tcx>>(
    s: &S,
    span: rustc_span::Span,
    konst: mir::Const<'tcx>,
) -> ConstOperandKind {
    use ConstOperandKind::{Promoted, Value};
    use rustc_middle::mir::Const;
    let tcx = s.base().tcx;
    match konst {
        Const::Val(const_value, ty) => {
            let evaluated = const_value_to_constant_expr(s, ty, const_value, span);
            match evaluated.report_err() {
                Ok(val) => Value(val),
                Err(err) => {
                    warning!(
                        s[span], "Couldn't convert constant back to an expression";
                        {const_value, ty, err}
                    );
                    Value(
                        ConstantExprKind::Todo("ConstEvalVal".into())
                            .decorate(ty.sinto(s), span.sinto(s)),
                    )
                }
            }
        }
        Const::Ty(_ty, c) => Value(c.sinto(s)),
        Const::Unevaluated(ucv, ty) => {
            use crate::rustc_middle::query::Key;
            let span = span.substitute_dummy(
                tcx.def_ident_span(ucv.def)
                    .unwrap_or_else(|| ucv.def.default_span(tcx)),
            );
            match ucv.promoted {
                Some(promoted) => {
                    let item = translate_item_ref(s, ucv.def, ucv.args);
                    let item = item.mutate_def_id(s, |def_id| {
                        // Construct a def_id for the promoted constant.
                        *def_id = def_id.make_promoted_child(s, promoted.sinto(s));
                    });
                    Promoted(item)
                }
                None => Value(match translate_constant_reference(s, span, ucv.shrink()) {
                    Some(val) => val,
                    None => match eval_mir_constant(s, konst) {
                        Some(val) => val.sinto(s),
                        // TODO: This is triggered when compiling using `generic_const_exprs`. We
                        // might be able to get a MIR body from the def_id.
                        None => ConstantExprKind::Todo("TranslateUneval".into())
                            .decorate(ty.sinto(s), span.sinto(s)),
                    },
                }),
            }
        }
    }
}

#[cfg(feature = "rustc")]
/// This impl is used in THIR patterns.
impl<'tcx, S: UnderOwnerState<'tcx>> SInto<S, ConstantExpr> for rustc_middle::mir::Const<'tcx> {
    fn sinto(&self, s: &S) -> ConstantExpr {
        match translate_mir_const(s, rustc_span::DUMMY_SP, *self) {
            ConstOperandKind::Value(val) => val,
            ConstOperandKind::Promoted(p) => {
                warning!(
                    s, "Couldn't convert constant back to an expression, expected value, got promoted";
                    {self, p}
                );

                ConstantExprKind::Todo("ConstEvalPromoted".into())
                    .decorate(self.ty().sinto(s), rustc_span::DUMMY_SP.sinto(s))
            }
        }
    }
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::Body<'tcx>, state: S as s)]
pub struct MirBody<KIND> {
    pub span: Span,
    #[map({
        x.iter_enumerated().map(|(local, local_decl)| {
            let mut local_decl = local_decl.sinto(s);
            local_decl.name = name_of_local(local, &self.var_debug_info);
            local_decl
        }).collect()
    })]
    pub local_decls: IndexVec<Local, LocalDecl>,
    pub arg_count: usize,
    pub basic_blocks: BasicBlocks,
    pub source_scopes: IndexVec<SourceScope, SourceScopeData>,
    pub tainted_by_errors: Option<ErrorGuaranteed>,
    #[value(std::marker::PhantomData)]
    pub _kind: std::marker::PhantomData<KIND>,
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx>>, from: rustc_middle::mir::SourceScopeData<'tcx>, state: S as s)]
pub struct SourceScopeData {
    pub span: Span,
    pub parent_scope: Option<SourceScope>,
    pub inlined_parent_scope: Option<SourceScope>,
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::Operand<'tcx>, state: S as s)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(ConstOperand),
}

#[cfg(feature = "rustc")]
impl Operand {
    pub(crate) fn ty(&self) -> &Ty {
        match self {
            Operand::Copy(p) | Operand::Move(p) => &p.ty,
            Operand::Constant(c) => &c.ty,
        }
    }
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::Terminator<'tcx>, state: S as s)]
pub struct Terminator {
    pub source_info: SourceInfo,
    pub kind: TerminatorKind,
}

#[cfg(feature = "rustc")]
fn translate_terminator_kind_call<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>(
    s: &S,
    terminator: &rustc_middle::mir::TerminatorKind<'tcx>,
) -> TerminatorKind {
    let tcx = s.base().tcx;
    let mir::TerminatorKind::Call {
        func,
        args,
        destination,
        target,
        unwind,
        fn_span,
        ..
    } = terminator
    else {
        unreachable!()
    };

    let ty = func.ty(&s.mir().local_decls, tcx);
    let hax_ty: crate::Ty = ty.sinto(s);
    let sig = match hax_ty.kind() {
        TyKind::Arrow(sig) => sig,
        TyKind::FnDef { fn_sig, .. } => fn_sig,
        TyKind::Closure(args) => &args.fn_sig,
        _ => supposely_unreachable_fatal!(
            s,
            "TerminatorKind_Call_expected_fn_type";
            { ty }
        ),
    };
    let fun_op = if let ty::TyKind::FnDef(def_id, generics) = ty.kind() {
        // The type of the value is one of the singleton types that corresponds to each function,
        // which is enough information.
        let item = translate_item_ref(s, *def_id, *generics);
        FunOperand::Static(item)
    } else {
        use mir::Operand;
        match func {
            Operand::Constant(_) => {
                unimplemented!("{:?}", func);
            }
            Operand::Move(place) => {
                // Function pointer or closure.
                let place = place.sinto(s);
                FunOperand::DynamicMove(place)
            }
            Operand::Copy(_place) => {
                unimplemented!("{:?}", func);
            }
        }
    };

    let late_bound_generics = sig
        .bound_vars
        .iter()
        .map(|var| match var {
            BoundVariableKind::Region(r) => r,
            BoundVariableKind::Ty(..) | BoundVariableKind::Const => {
                supposely_unreachable_fatal!(
                    s,
                    "non_lifetime_late_bound";
                    { var }
                )
            }
        })
        .map(|_| {
            GenericArg::Lifetime(Region {
                kind: RegionKind::ReErased,
            })
        })
        .collect();
    TerminatorKind::Call {
        fun: fun_op,
        late_bound_generics,
        args: args.sinto(s),
        destination: destination.sinto(s),
        target: target.sinto(s),
        unwind: unwind.sinto(s),
        fn_span: fn_span.sinto(s),
    }
}

#[cfg(feature = "rustc")]
fn translate_terminator_kind_drop<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>(
    s: &S,
    terminator: &rustc_middle::mir::TerminatorKind<'tcx>,
) -> TerminatorKind {
    let tcx = s.base().tcx;
    let mir::TerminatorKind::Drop {
        place,
        target,
        unwind,
        ..
    } = terminator
    else {
        unreachable!()
    };

    let local_decls = &s.mir().local_decls;
    let place_ty = place.ty(local_decls, tcx).ty;
    let drop_trait = tcx.lang_items().drop_trait().unwrap();
    let impl_expr = solve_trait(
        s,
        ty::Binder::dummy(ty::TraitRef::new(tcx, drop_trait, [place_ty])),
    );

    TerminatorKind::Drop {
        place: place.sinto(s),
        impl_expr,
        target: target.sinto(s),
        unwind: unwind.sinto(s),
    }
}

// We don't use the LitIntType on purpose (we don't want the "unsuffixed" case)
#[derive_group(Serializers)]
#[derive(Clone, Copy, Debug, JsonSchema, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScalarTy {
    Bool,
    Int(IntTy),
    Uint(UintTy),
    Char,
}

#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub struct ScalarInt {
    /// Little-endian representation of the integer
    pub data_le_bytes: [u8; 16],
    pub int_ty: ScalarTy,
}

/// Translate a `SwitchInt` terminator.
#[cfg(feature = "rustc")]
fn translate_switchint<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>(
    s: &S,
    discr: &mir::Operand<'tcx>,
    targets: &mir::SwitchTargets,
) -> TerminatorKind {
    let discr = discr.sinto(s);
    let ty = match discr.ty().kind() {
        TyKind::Bool => ScalarTy::Bool,
        TyKind::Int(ty) => ScalarTy::Int(*ty),
        TyKind::Uint(ty) => ScalarTy::Uint(*ty),
        TyKind::Char => ScalarTy::Char,
        ty => fatal!(s, "Unexpected switch_ty: {:?}", ty),
    };

    // Convert all the test values to the proper values.
    let otherwise = targets.otherwise().sinto(s);
    let targets_vec: Vec<(ScalarInt, BasicBlock)> = targets
        .iter()
        .map(|(v, b)| {
            let v = ScalarInt {
                data_le_bytes: v.to_le_bytes(),
                int_ty: ty,
            };
            (v, b.sinto(s))
        })
        .collect();

    TerminatorKind::SwitchInt {
        discr,
        ty,
        targets: targets_vec,
        otherwise,
    }
}

/// A value of type `fn<...> A -> B` that can be called.
#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub enum FunOperand {
    /// Call to a statically-known function.
    Static(ItemRef),
    /// Use of a closure or a function pointer value. Counts as a move from the given place.
    DynamicMove(Place),
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: BaseState<'tcx>>, from: rustc_middle::mir::UnwindAction, state: S as _s)]
pub enum UnwindAction {
    Continue,
    Unreachable,
    Terminate(UnwindTerminateReason),
    Cleanup(BasicBlock),
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::TerminatorKind<'tcx>, state: S as s)]
pub enum TerminatorKind {
    Goto {
        target: BasicBlock,
    },
    #[custom_arm(
        rustc_middle::mir::TerminatorKind::SwitchInt { discr, targets } => {
            translate_switchint(s, discr, targets)
        }
    )]
    SwitchInt {
        /// The value being switched one.
        discr: Operand,
        /// The type that is being switched on.
        ty: ScalarTy,
        /// Possible success cases.
        targets: Vec<(ScalarInt, BasicBlock)>,
        /// If none of the `targets` match, branch to that block.
        otherwise: BasicBlock,
    },
    Return,
    Unreachable,
    #[custom_arm(
        x @ rustc_middle::mir::TerminatorKind::Drop { .. } => {
          translate_terminator_kind_drop(s, x)
        }
    )]
    Drop {
        place: Place,
        /// Implementation of `place.ty(): Drop`.
        impl_expr: ImplExpr,
        target: BasicBlock,
        unwind: UnwindAction,
    },
    #[custom_arm(
        x @ rustc_middle::mir::TerminatorKind::Call { .. } => {
          translate_terminator_kind_call(s, x)
        }
    )]
    Call {
        fun: FunOperand,
        /// A `FunOperand` is a value of type `fn<...> A -> B`. The generics in `<...>` are called
        /// "late-bound" and are instantiated anew at each call site. This list provides the
        /// generics used at this call-site. They are all lifetimes and at the time of writing are
        /// all erased lifetimes.
        late_bound_generics: Vec<GenericArg>,
        args: Vec<Spanned<Operand>>,
        destination: Place,
        target: Option<BasicBlock>,
        unwind: UnwindAction,
        fn_span: Span,
    },
    TailCall {
        func: Operand,
        args: Vec<Spanned<Operand>>,
        fn_span: Span,
    },
    Assert {
        cond: Operand,
        expected: bool,
        msg: AssertMessage,
        target: BasicBlock,
        unwind: UnwindAction,
    },
    Yield {
        value: Operand,
        resume: BasicBlock,
        resume_arg: Place,
        drop: Option<BasicBlock>,
    },
    CoroutineDrop,
    FalseEdge {
        real_target: BasicBlock,
        imaginary_target: BasicBlock,
    },
    FalseUnwind {
        real_target: BasicBlock,
        unwind: UnwindAction,
    },
    UnwindResume,
    UnwindTerminate(UnwindTerminateReason),
    InlineAsm {
        template: Vec<InlineAsmTemplatePiece>,
        operands: Vec<InlineAsmOperand>,
        options: InlineAsmOptions,
        line_spans: Vec<Span>,
        targets: Vec<BasicBlock>,
        unwind: UnwindAction,
    },
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::Statement<'tcx>, state: S as s)]
pub struct Statement {
    pub source_info: SourceInfo,
    #[map(Box::new(x.sinto(s)))]
    pub kind: Box<StatementKind>,
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::StatementKind<'tcx>, state: S as s)]
pub enum StatementKind {
    Assign((Place, Rvalue)),
    FakeRead((FakeReadCause, Place)),
    SetDiscriminant {
        place: Place,
        variant_index: VariantIdx,
    },
    Deinit(Place),
    StorageLive(Local),
    StorageDead(Local),
    Retag(RetagKind, Place),
    PlaceMention(Place),
    AscribeUserType((Place, UserTypeProjection), Variance),
    Coverage(CoverageKind),
    Intrinsic(NonDivergingIntrinsic),
    ConstEvalCounter,
    BackwardIncompatibleDropHint {
        place: Place,
    },
    Nop,
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::NonDivergingIntrinsic<'tcx>, state: S as s)]
pub enum NonDivergingIntrinsic {
    Assume(Operand),
    CopyNonOverlapping(CopyNonOverlapping),
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::CopyNonOverlapping<'tcx>, state: S as s)]
pub struct CopyNonOverlapping {
    pub src: Operand,
    pub dst: Operand,
    pub count: Operand,
}

#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub struct Place {
    /// The type of the element on which we apply the projection given by `kind`
    pub ty: Ty,
    pub kind: PlaceKind,
}

#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub enum PlaceKind {
    Local(Local),
    Projection {
        place: Box<Place>,
        kind: ProjectionElem,
    },
}

#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub enum ProjectionElemFieldKind {
    Tuple(FieldIdx),
    Adt {
        typ: DefId,
        variant: Option<VariantIdx>,
        index: FieldIdx,
    },
    /// Get access to one of the fields of the state of a closure
    ClosureState(FieldIdx),
}

#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub enum ProjectionElem {
    Deref,
    Field(ProjectionElemFieldKind),
    Index(Local),
    ConstantIndex {
        offset: u64,
        min_length: u64,
        from_end: bool,
    },
    Subslice {
        from: u64,
        to: u64,
        from_end: bool,
    },
    Downcast(Option<Symbol>, VariantIdx),
    OpaqueCast,
}

// refactor
#[cfg(feature = "rustc")]
impl<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>> SInto<S, Place>
    for rustc_middle::mir::Place<'tcx>
{
    #[tracing::instrument(level = "info", skip(s))]
    fn sinto(&self, s: &S) -> Place {
        let tcx = s.base().tcx;
        let local_decls = &s.mir().local_decls;

        let mut place_ty: mir::PlaceTy = mir::Place::from(self.local).ty(local_decls, tcx);
        let mut place = Place {
            ty: place_ty.ty.sinto(s),
            kind: PlaceKind::Local(self.local.sinto(s)),
        };
        for elem in self.projection.as_slice() {
            use rustc_middle::mir::ProjectionElem::*;
            let projected_place_ty = place_ty.projection_ty(tcx, *elem);
            if matches!(elem, Downcast { .. }) {
                // We keep the same `Place`, the variant is tracked in the `PlaceTy` and we can
                // access it next loop iteration.
            } else {
                let elem_kind = match elem {
                    Deref => ProjectionElem::Deref,
                    Field(index, _) => {
                        let field_pj = match place_ty.ty.kind() {
                            ty::Adt(adt_def, _) => {
                                let variant = place_ty.variant_index;
                                assert!(
                                    ((adt_def.is_struct() || adt_def.is_union())
                                        && variant.is_none())
                                        || (adt_def.is_enum() && variant.is_some())
                                );
                                ProjectionElemFieldKind::Adt {
                                    typ: adt_def.did().sinto(s),
                                    variant: variant.map(|id| id.sinto(s)),
                                    index: index.sinto(s),
                                }
                            }
                            ty::Tuple(_types) => ProjectionElemFieldKind::Tuple(index.sinto(s)),
                            // We get there when we access one of the fields of the the state
                            // captured by a closure.
                            ty::Closure(..) => {
                                ProjectionElemFieldKind::ClosureState(index.sinto(s))
                            }
                            ty_kind => supposely_unreachable_fatal!(
                                s, "ProjectionElemFieldBadType";
                                {index, ty_kind, &place_ty, &place}
                            ),
                        };
                        ProjectionElem::Field(field_pj)
                    }
                    Index(local) => ProjectionElem::Index(local.sinto(s)),
                    ConstantIndex {
                        offset,
                        min_length,
                        from_end,
                    } => ProjectionElem::ConstantIndex {
                        offset: *offset,
                        min_length: *min_length,
                        from_end: *from_end,
                    },
                    Subslice { from, to, from_end } => ProjectionElem::Subslice {
                        from: *from,
                        to: *to,
                        from_end: *from_end,
                    },
                    OpaqueCast(..) => ProjectionElem::OpaqueCast,
                    // This is used for casts to a subtype, e.g. between `for<‘a> fn(&’a ())`
                    // and `fn(‘static ())` (according to @compiler-errors on Zulip).
                    Subtype { .. } => panic!("unexpected Subtype"),
                    Downcast { .. } => unreachable!(),
                    UnwrapUnsafeBinder { .. } => panic!("unsupported feature: unsafe binders"),
                };

                place = Place {
                    ty: projected_place_ty.ty.sinto(s),
                    kind: PlaceKind::Projection {
                        place: Box::new(place),
                        kind: elem_kind,
                    },
                };
            }
            place_ty = projected_place_ty;
        }
        place
    }
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::AggregateKind<'tcx>, state: S as s)]
pub enum AggregateKind {
    Array(Ty),
    Tuple,
    #[custom_arm(rustc_middle::mir::AggregateKind::Adt(def_id, vid, generics, annot, fid) => {
        let adt_kind = s.base().tcx.adt_def(def_id).adt_kind().sinto(s);
        let item = translate_item_ref(s, *def_id, generics);
        AggregateKind::Adt(
            item,
            vid.sinto(s),
            adt_kind,
            annot.sinto(s),
            fid.sinto(s),
        )
    })]
    Adt(
        ItemRef,
        VariantIdx,
        AdtKind,
        Option<UserTypeAnnotationIndex>,
        Option<FieldIdx>,
    ),
    #[custom_arm(rustc_middle::mir::AggregateKind::Closure(def_id, generics) => {
        let closure = generics.as_closure();
        let args = ClosureArgs::sfrom(s, *def_id, closure);
        AggregateKind::Closure(args)
    })]
    Closure(ClosureArgs),
    #[custom_arm(FROM_TYPE::Coroutine(def_id, generics) => TO_TYPE::Coroutine(translate_item_ref(s, *def_id, generics)),)]
    Coroutine(ItemRef),
    #[custom_arm(FROM_TYPE::CoroutineClosure(def_id, generics) => TO_TYPE::CoroutineClosure(translate_item_ref(s, *def_id, generics)),)]
    CoroutineClosure(ItemRef),
    RawPtr(Ty, Mutability),
}

#[derive_group(Serializers)]
#[derive(Clone, Debug, JsonSchema)]
pub enum CastKind {
    PointerExposeProvenance,
    PointerWithExposedProvenance,
    PointerCoercion(PointerCoercion, CoercionSource),
    IntToInt,
    FloatToInt,
    FloatToFloat,
    IntToFloat,
    PtrToPtr,
    FnPtrToPtr,
    Transmute,
}

#[cfg(feature = "rustc")]
impl CastKind {
    fn sfrom<'tcx, S: UnderOwnerState<'tcx>>(
        s: &S,
        kind: mir::CastKind,
        src_ty: ty::Ty<'tcx>,
        tgt_ty: ty::Ty<'tcx>,
    ) -> CastKind {
        match kind {
            mir::CastKind::PointerExposeProvenance => CastKind::PointerExposeProvenance,
            mir::CastKind::PointerWithExposedProvenance => CastKind::PointerWithExposedProvenance,
            mir::CastKind::PointerCoercion(coercion, y) => {
                let coercion = PointerCoercion::sfrom(s, coercion, src_ty, tgt_ty);
                CastKind::PointerCoercion(coercion, y.sinto(s))
            }
            mir::CastKind::IntToInt => CastKind::IntToInt,
            mir::CastKind::FloatToInt => CastKind::FloatToInt,
            mir::CastKind::FloatToFloat => CastKind::FloatToFloat,
            mir::CastKind::IntToFloat => CastKind::IntToFloat,
            mir::CastKind::PtrToPtr => CastKind::PtrToPtr,
            mir::CastKind::FnPtrToPtr => CastKind::FnPtrToPtr,
            mir::CastKind::Transmute => CastKind::Transmute,
        }
    }
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S>, from: rustc_middle::mir::CoercionSource, state: S as _s)]
pub enum CoercionSource {
    AsCast,
    Implicit,
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::NullOp<'tcx>, state: S as s)]
pub enum NullOp {
    SizeOf,
    AlignOf,
    OffsetOf(Vec<(VariantIdx, FieldIdx)>),
    UbChecks,
    ContractChecks,
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::Rvalue<'tcx>, state: S as s)]
pub enum Rvalue {
    Use(Operand),
    Repeat(Operand, ConstantExpr),
    Ref(Region, BorrowKind, Place),
    ThreadLocalRef(DefId),
    RawPtr(RawPtrKind, Place),
    Len(Place),
    #[custom_arm(
        FROM_TYPE::Cast(kind, op, tgt_ty) => {
            let src_ty = op.ty(&*s.mir(), s.base().tcx);
            let kind = CastKind::sfrom(s, *kind, src_ty, *tgt_ty);
            TO_TYPE::Cast(kind, op.sinto(s), tgt_ty.sinto(s))
        },
    )]
    Cast(CastKind, Operand, Ty),
    BinaryOp(BinOp, (Operand, Operand)),
    NullaryOp(NullOp, Ty),
    UnaryOp(UnOp, Operand),
    Discriminant(Place),
    Aggregate(AggregateKind, IndexVec<FieldIdx, Operand>),
    ShallowInitBox(Operand, Ty),
    CopyForDeref(Place),
    WrapUnsafeBinder(Operand, Ty),
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: BaseState<'tcx>>, from: rustc_middle::mir::RawPtrKind, state: S as _s)]
pub enum RawPtrKind {
    Mut,
    Const,
    FakeForPtrMetadata,
}

#[derive_group(Serializers)]
#[derive(AdtInto, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: UnderOwnerState<'tcx> + HasMir<'tcx>>, from: rustc_middle::mir::BasicBlockData<'tcx>, state: S as s)]
pub struct BasicBlockData {
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
    pub is_cleanup: bool,
}

make_idx_wrapper!(rustc_middle::mir, BasicBlock);
make_idx_wrapper!(rustc_middle::mir, SourceScope);
make_idx_wrapper!(rustc_middle::mir, Local);
make_idx_wrapper!(rustc_middle::ty, UserTypeAnnotationIndex);
make_idx_wrapper!(rustc_abi, FieldIdx);

/// Reflects [`rustc_middle::mir::UnOp`]
#[derive_group(Serializers)]
#[derive(AdtInto, Copy, Clone, Debug, JsonSchema)]
#[args(<'slt, S: UnderOwnerState<'slt>>, from: mir::UnOp, state: S as _s)]
pub enum UnOp {
    Not,
    Neg,
    PtrMetadata,
}

/// Reflects [`rustc_middle::mir::BinOp`]
#[derive_group(Serializers)]
#[derive(AdtInto, Copy, Clone, Debug, JsonSchema)]
#[args(<'slt, S: UnderOwnerState<'slt>>, from: mir::BinOp, state: S as _s)]
pub enum BinOp {
    Add,
    AddUnchecked,
    AddWithOverflow,
    Sub,
    SubUnchecked,
    SubWithOverflow,
    Mul,
    MulUnchecked,
    MulWithOverflow,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    ShlUnchecked,
    Shr,
    ShrUnchecked,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    Cmp,
    Offset,
}

/// Reflects [`rustc_middle::mir::AssignOp`]
#[derive_group(Serializers)]
#[derive(AdtInto, Copy, Clone, Debug, JsonSchema)]
#[args(<'tcx, S: BaseState<'tcx>>, from: mir::AssignOp, state: S as _s)]
pub enum AssignOp {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitXorAssign,
    BitAndAssign,
    BitOrAssign,
    ShlAssign,
    ShrAssign,
}

/// Reflects [`rustc_middle::mir::BorrowKind`]
#[derive(AdtInto)]
#[args(<S>, from: mir::BorrowKind, state: S as gstate)]
#[derive_group(Serializers)]
#[derive(Copy, Clone, Debug, JsonSchema, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BorrowKind {
    Shared,
    Fake(FakeBorrowKind),
    Mut { kind: MutBorrowKind },
}

/// Reflects [`rustc_middle::mir::MutBorrowKind`]
#[derive(AdtInto)]
#[args(<S>, from: rustc_middle::mir::MutBorrowKind, state: S as _s)]
#[derive_group(Serializers)]
#[derive(Copy, Clone, Debug, JsonSchema, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum MutBorrowKind {
    Default,
    TwoPhaseBorrow,
    ClosureCapture,
}

/// Reflects [`rustc_middle::mir::FakeBorrowKind`]
#[derive(AdtInto)]
#[args(<S>, from: rustc_middle::mir::FakeBorrowKind, state: S as _s)]
#[derive_group(Serializers)]
#[derive(Copy, Clone, Debug, JsonSchema, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum FakeBorrowKind {
    /// A shared (deep) borrow. Data must be immutable and is aliasable.
    Deep,
    /// The immediately borrowed place must be immutable, but projections from
    /// it don't need to be. This is used to prevent match guards from replacing
    /// the scrutinee. For example, a fake borrow of `a.b` doesn't
    /// conflict with a mutable borrow of `a.b.c`.
    Shallow,
}

sinto_todo!(rustc_ast::ast, InlineAsmTemplatePiece);
sinto_todo!(rustc_ast::ast, InlineAsmOptions);
sinto_todo!(rustc_middle::mir, InlineAsmOperand<'tcx>);
sinto_todo!(rustc_middle::mir, AssertMessage<'tcx>);
sinto_todo!(rustc_middle::mir, FakeReadCause);
sinto_todo!(rustc_middle::mir, RetagKind);
sinto_todo!(rustc_middle::mir, UserTypeProjection);
sinto_todo!(rustc_middle::mir, UnwindTerminateReason);
sinto_todo!(rustc_middle::mir::coverage, CoverageKind);
