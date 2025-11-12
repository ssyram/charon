{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Expressions.hs`
by hand. Edit `templates/Expressions.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Expressions where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values
import Generated_Types

-- | An aggregated ADT.
-- | 
-- | Note that ADTs are desaggregated at some point in MIR. For instance, if
-- | we have in Rust:
-- | ```ignore
-- |   let ls = Cons(hd, tl);
-- | ```
-- | 
-- | In MIR we have (yes, the discriminant update happens *at the end* for some
-- | reason):
-- | ```text
-- |   (ls as Cons).0 = move hd;
-- |   (ls as Cons).1 = move tl;
-- |   discriminant(ls) = 0; // assuming `Cons` is the variant of index 0
-- | ```
-- | 
-- | Rem.: in the Aeneas semantics, both cases are handled (in case of desaggregated
-- | initialization, `ls` is initialized to `⊥`, then this `⊥` is expanded to
-- | `Cons (⊥, ⊥)` upon the first assignment, at which point we can initialize
-- | the field 0, etc.).
data AggregateKind = AggregatedAdt TypeDeclRef Maybe VariantId Maybe FieldId
  | AggregatedArray Ty ConstGeneric
  | AggregatedRawPtr Ty RefKind
  deriving (Show, Eq, Ord)

-- | Binary operations.
data Binop = BitXor
  | BitAnd
  | BitOr
  | Eq
  | Lt
  | Le
  | Ne
  | Ge
  | Gt
  | Add OverflowMode
  | Sub OverflowMode
  | Mul OverflowMode
  | Div OverflowMode
  | Rem OverflowMode
  | AddChecked
  | SubChecked
  | MulChecked
  | Shl OverflowMode
  | Shr OverflowMode
  | Offset
  | Cmp
  deriving (Show, Eq, Ord)

data BorrowKind = BShared
  | BMut
  | BTwoPhaseMut
  | BShallow
  | BUniqueImmutable
  deriving (Show, Eq, Ord)

-- | For all the variants: the first type gives the source type, the second one gives
-- | the destination type.
data CastKind = CastScalar LiteralType LiteralType
  | CastRawPtr Ty Ty
  | CastFnPtr Ty Ty
  | CastUnsize Ty Ty UnsizingMetadata
  | CastTransmute Ty Ty
  | CastConcretize Ty Ty
  deriving (Show, Eq, Ord)

data ConstantExpr = ConstantExpr
  { kind :: ConstantExprKind
  , ty :: Ty
  }
  deriving (Show, Eq, Ord)

-- | A constant expression.
-- | 
-- | Only the [`ConstantExprKind::Literal`] and [`ConstantExprKind::Var`]
-- | cases are left in the final LLBC.
-- | 
-- | The other cases come from a straight translation from the MIR:
-- | 
-- | [`ConstantExprKind::Adt`] case:
-- | It is a bit annoying, but rustc treats some ADT and tuple instances as
-- | constants when generating MIR:
-- | - an enumeration with one variant and no fields is a constant.
-- | - a structure with no field is a constant.
-- | - sometimes, Rust stores the initialization of an ADT as a constant
-- |   (if all the fields are constant) rather than as an aggregated value
-- | We later desugar those to regular ADTs, see [regularize_constant_adts.rs].
-- | 
-- | [`ConstantExprKind::Global`] case: access to a global variable. We later desugar it to
-- | a copy of a place global.
-- | 
-- | [`ConstantExprKind::Ref`] case: reference to a constant value. We later desugar it to a separate
-- | statement.
-- | 
-- | [`ConstantExprKind::FnPtr`] case: a function pointer (to a top-level function).
-- | 
-- | Remark:
-- | MIR seems to forbid more complex expressions like paths. For instance,
-- | reading the constant `a.b` is translated to `{ _1 = const a; _2 = (_1.0) }`.
data ConstantExprKind = CLiteral Literal
  | CTraitConst TraitRef TraitItemName
  | CVar (DeBruijnVar ConstGenericVarId)
  | CFnPtr FnPtr
  | CRawMemory [Int]
  | COpaque String
  deriving (Show, Eq, Ord)

data FieldProjKind = ProjAdt TypeDeclId Maybe VariantId
  | ProjTuple Int
  deriving (Show, Eq, Ord)

data LocalId = LocalId
  { raw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Nullary operation
data Nullop = SizeOf
  | AlignOf
  | OffsetOf [(Int, FieldId)]
  | UbChecks
  deriving (Show, Eq, Ord)

data Operand = Copy Place
  | Move Place
  | Constant ConstantExpr
  deriving (Show, Eq, Ord)

data OverflowMode = OPanic
  | Oub
  | OWrap
  deriving (Show, Eq, Ord)

data Place = Place
  { kind :: PlaceKind
  , ty :: Ty
  }
  deriving (Show, Eq, Ord)

data PlaceKind = PlaceLocal LocalId
  | PlaceProjection Place ProjectionElem
  | PlaceGlobal GlobalDeclRef
  deriving (Show, Eq, Ord)

-- | Note that we don't have the equivalent of "downcasts".
-- | Downcasts are actually necessary, for instance when initializing enumeration
-- | values: the value is initially `Bottom`, and we need a way of knowing the
-- | variant.
-- | For example:
-- | `((_0 as Right).0: T2) = move _1;`
-- | In MIR, downcasts always happen before field projections: in our internal
-- | language, we thus merge downcasts and field projections.
data ProjectionElem = Deref
  | Field FieldProjKind FieldId
  | PtrMetadata
  | ProjIndex Operand Bool
  | Subslice Operand Operand Bool
  deriving (Show, Eq, Ord)

-- | TODO: we could factor out [Rvalue] and function calls (for LLBC, not ULLBC).
-- | We can also factor out the unops, binops with the function calls.
-- | TODO: move the aggregate kind to operands
-- | TODO: we should prefix the type variants with "R" or "Rv", this would avoid collisions
data Rvalue = Use Operand
  | RvRef Place BorrowKind Operand
  | RawPtr Place RefKind Operand
  | BinaryOp Binop Operand Operand
  | UnaryOp Unop Operand
  | NullaryOp Nullop Ty
  | Discriminant Place
  | Aggregate AggregateKind [Operand]
  | Len Place Ty Maybe ConstGeneric
  | Repeat Operand Ty ConstGeneric
  | ShallowInitBox Operand Ty
  deriving (Show, Eq, Ord)

-- | Unary operation
data Unop = Not
  | Neg OverflowMode
  | Cast CastKind
  deriving (Show, Eq, Ord)

data UnsizingMetadata = MetaLength ConstGeneric
  | MetaVTablePtr TraitRef
  | MetaUnknown
  deriving (Show, Eq, Ord)
