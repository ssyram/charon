{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Expressions.hs`
by hand. Edit `templates/Expressions.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Expressions where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import Generated_Meta
import Generated_Values
import Generated_Types hiding (Field, PtrMetadata)

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
data AggregateKind = AggregatedAdt TypeDeclRef (Maybe VariantId) (Maybe FieldId)
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
  { constantexprKind :: ConstantExprKind
  , constantexprTy :: Ty
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
  | CVar ((DeBruijnVar ConstGenericVarId))
  | CFnPtr FnPtr
  | CRawMemory [Int]
  | COpaque String
  deriving (Show, Eq, Ord)

data FieldProjKind = ProjAdt TypeDeclId (Maybe VariantId)
  | ProjTuple Int
  deriving (Show, Eq, Ord)

data LocalId = LocalId
  { localidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Nullary operation
data Nullop = SizeOf
  | AlignOf
  | OffsetOf ([(Int, FieldId)])
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
  { placeKind :: PlaceKind
  , placeTy :: Ty
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
  | Len Place Ty (Maybe ConstGeneric)
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

instance FromJSON AggregateKind where
  parseJSON v = case v of
    Object o | H.lookup "Adt" o /= Nothing -> do
      withArray "AggregatedAdt" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (AggregatedAdt v0 v1 v2)) =<< o .: "Adt"
    Object o | H.lookup "Array" o /= Nothing -> do
      withArray "AggregatedArray" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (AggregatedArray v0 v1)) =<< o .: "Array"
    Object o | H.lookup "RawPtr" o /= Nothing -> do
      withArray "AggregatedRawPtr" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (AggregatedRawPtr v0 v1)) =<< o .: "RawPtr"
    _ -> fail "Unknown variant"


instance FromJSON Binop where
  parseJSON v = case v of
    String "BitXor" -> pure BitXor
    String "BitAnd" -> pure BitAnd
    String "BitOr" -> pure BitOr
    String "Eq" -> pure Eq
    String "Lt" -> pure Lt
    String "Le" -> pure Le
    String "Ne" -> pure Ne
    String "Ge" -> pure Ge
    String "Gt" -> pure Gt
    Object o | H.lookup "Add" o /= Nothing -> do
      v <- o .: "Add"
      Add <$> parseJSON v
    Object o | H.lookup "Sub" o /= Nothing -> do
      v <- o .: "Sub"
      Sub <$> parseJSON v
    Object o | H.lookup "Mul" o /= Nothing -> do
      v <- o .: "Mul"
      Mul <$> parseJSON v
    Object o | H.lookup "Div" o /= Nothing -> do
      v <- o .: "Div"
      Div <$> parseJSON v
    Object o | H.lookup "Rem" o /= Nothing -> do
      v <- o .: "Rem"
      Rem <$> parseJSON v
    String "AddChecked" -> pure AddChecked
    String "SubChecked" -> pure SubChecked
    String "MulChecked" -> pure MulChecked
    Object o | H.lookup "Shl" o /= Nothing -> do
      v <- o .: "Shl"
      Shl <$> parseJSON v
    Object o | H.lookup "Shr" o /= Nothing -> do
      v <- o .: "Shr"
      Shr <$> parseJSON v
    String "Offset" -> pure Offset
    String "Cmp" -> pure Cmp
    _ -> fail "Unknown variant"


instance FromJSON BorrowKind where
  parseJSON v = case v of
    String "Shared" -> pure BShared
    String "Mut" -> pure BMut
    String "TwoPhaseMut" -> pure BTwoPhaseMut
    String "Shallow" -> pure BShallow
    String "UniqueImmutable" -> pure BUniqueImmutable
    _ -> fail "Unknown variant"


instance FromJSON CastKind where
  parseJSON v = case v of
    Object o | H.lookup "Scalar" o /= Nothing -> do
      withArray "CastScalar" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (CastScalar v0 v1)) =<< o .: "Scalar"
    Object o | H.lookup "RawPtr" o /= Nothing -> do
      withArray "CastRawPtr" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (CastRawPtr v0 v1)) =<< o .: "RawPtr"
    Object o | H.lookup "FnPtr" o /= Nothing -> do
      withArray "CastFnPtr" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (CastFnPtr v0 v1)) =<< o .: "FnPtr"
    Object o | H.lookup "Unsize" o /= Nothing -> do
      withArray "CastUnsize" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (CastUnsize v0 v1 v2)) =<< o .: "Unsize"
    Object o | H.lookup "Transmute" o /= Nothing -> do
      withArray "CastTransmute" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (CastTransmute v0 v1)) =<< o .: "Transmute"
    Object o | H.lookup "Concretize" o /= Nothing -> do
      withArray "CastConcretize" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (CastConcretize v0 v1)) =<< o .: "Concretize"
    _ -> fail "Unknown variant"


instance FromJSON ConstantExpr where
  parseJSON = withObject "ConstantExpr" $ \o -> do
    constantexprKind <- o .: "kind"
    constantexprTy <- o .: "ty"
    pure (ConstantExpr constantexprKind constantexprTy)


instance FromJSON ConstantExprKind where
  parseJSON v = case v of
    Object o | H.lookup "Literal" o /= Nothing -> do
      v <- o .: "Literal"
      CLiteral <$> parseJSON v
    Object o | H.lookup "TraitConst" o /= Nothing -> do
      withArray "CTraitConst" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (CTraitConst v0 v1)) =<< o .: "TraitConst"
    Object o | H.lookup "Var" o /= Nothing -> do
      v <- o .: "Var"
      CVar <$> parseJSON v
    Object o | H.lookup "FnPtr" o /= Nothing -> do
      v <- o .: "FnPtr"
      CFnPtr <$> parseJSON v
    Object o | H.lookup "RawMemory" o /= Nothing -> do
      v <- o .: "RawMemory"
      CRawMemory <$> parseJSON v
    Object o | H.lookup "Opaque" o /= Nothing -> do
      v <- o .: "Opaque"
      COpaque <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON FieldProjKind where
  parseJSON v = case v of
    Object o | H.lookup "Adt" o /= Nothing -> do
      withArray "ProjAdt" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (ProjAdt v0 v1)) =<< o .: "Adt"
    Object o | H.lookup "Tuple" o /= Nothing -> do
      v <- o .: "Tuple"
      ProjTuple <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON LocalId where
  parseJSON = fmap LocalId . parseJSON


instance FromJSON Nullop where
  parseJSON v = case v of
    String "SizeOf" -> pure SizeOf
    String "AlignOf" -> pure AlignOf
    Object o | H.lookup "OffsetOf" o /= Nothing -> do
      v <- o .: "OffsetOf"
      OffsetOf <$> parseJSON v
    String "UbChecks" -> pure UbChecks
    _ -> fail "Unknown variant"


instance FromJSON Operand where
  parseJSON v = case v of
    Object o | H.lookup "Copy" o /= Nothing -> do
      v <- o .: "Copy"
      Copy <$> parseJSON v
    Object o | H.lookup "Move" o /= Nothing -> do
      v <- o .: "Move"
      Move <$> parseJSON v
    Object o | H.lookup "Const" o /= Nothing -> do
      v <- o .: "Const"
      Constant <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON OverflowMode where
  parseJSON v = case v of
    String "Panic" -> pure OPanic
    String "UB" -> pure Oub
    String "Wrap" -> pure OWrap
    _ -> fail "Unknown variant"


instance FromJSON Place where
  parseJSON = withObject "Place" $ \o -> do
    placeKind <- o .: "kind"
    placeTy <- o .: "ty"
    pure (Place placeKind placeTy)


instance FromJSON PlaceKind where
  parseJSON v = case v of
    Object o | H.lookup "Local" o /= Nothing -> do
      v <- o .: "Local"
      PlaceLocal <$> parseJSON v
    Object o | H.lookup "Projection" o /= Nothing -> do
      withArray "PlaceProjection" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (PlaceProjection v0 v1)) =<< o .: "Projection"
    Object o | H.lookup "Global" o /= Nothing -> do
      v <- o .: "Global"
      PlaceGlobal <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON ProjectionElem where
  parseJSON v = case v of
    String "Deref" -> pure Deref
    Object o | H.lookup "Field" o /= Nothing -> do
      withArray "Field" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (Field v0 v1)) =<< o .: "Field"
    String "PtrMetadata" -> pure PtrMetadata
    Object o | H.lookup "Index" o /= Nothing -> do
      obj <- o .: "Index"
      offset <- obj .: "offset"
      fromEnd <- obj .: "from_end"
      pure (ProjIndex offset fromEnd)
    Object o | H.lookup "Subslice" o /= Nothing -> do
      obj <- o .: "Subslice"
      from <- obj .: "from"
      to <- obj .: "to"
      fromEnd <- obj .: "from_end"
      pure (Subslice from to fromEnd)
    _ -> fail "Unknown variant"


instance FromJSON Rvalue where
  parseJSON v = case v of
    Object o | H.lookup "Use" o /= Nothing -> do
      v <- o .: "Use"
      Use <$> parseJSON v
    Object o | H.lookup "Ref" o /= Nothing -> do
      obj <- o .: "Ref"
      place <- obj .: "place"
      kind <- obj .: "kind"
      ptrMetadata <- obj .: "ptr_metadata"
      pure (RvRef place kind ptrMetadata)
    Object o | H.lookup "RawPtr" o /= Nothing -> do
      obj <- o .: "RawPtr"
      place <- obj .: "place"
      kind <- obj .: "kind"
      ptrMetadata <- obj .: "ptr_metadata"
      pure (RawPtr place kind ptrMetadata)
    Object o | H.lookup "BinaryOp" o /= Nothing -> do
      withArray "BinaryOp" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (BinaryOp v0 v1 v2)) =<< o .: "BinaryOp"
    Object o | H.lookup "UnaryOp" o /= Nothing -> do
      withArray "UnaryOp" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (UnaryOp v0 v1)) =<< o .: "UnaryOp"
    Object o | H.lookup "NullaryOp" o /= Nothing -> do
      withArray "NullaryOp" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (NullaryOp v0 v1)) =<< o .: "NullaryOp"
    Object o | H.lookup "Discriminant" o /= Nothing -> do
      v <- o .: "Discriminant"
      Discriminant <$> parseJSON v
    Object o | H.lookup "Aggregate" o /= Nothing -> do
      withArray "Aggregate" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (Aggregate v0 v1)) =<< o .: "Aggregate"
    Object o | H.lookup "Len" o /= Nothing -> do
      withArray "Len" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (Len v0 v1 v2)) =<< o .: "Len"
    Object o | H.lookup "Repeat" o /= Nothing -> do
      withArray "Repeat" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (Repeat v0 v1 v2)) =<< o .: "Repeat"
    Object o | H.lookup "ShallowInitBox" o /= Nothing -> do
      withArray "ShallowInitBox" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (ShallowInitBox v0 v1)) =<< o .: "ShallowInitBox"
    _ -> fail "Unknown variant"


instance FromJSON Unop where
  parseJSON v = case v of
    String "Not" -> pure Not
    Object o | H.lookup "Neg" o /= Nothing -> do
      v <- o .: "Neg"
      Neg <$> parseJSON v
    Object o | H.lookup "Cast" o /= Nothing -> do
      v <- o .: "Cast"
      Cast <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON UnsizingMetadata where
  parseJSON v = case v of
    Object o | H.lookup "Length" o /= Nothing -> do
      v <- o .: "Length"
      MetaLength <$> parseJSON v
    Object o | H.lookup "VTablePtr" o /= Nothing -> do
      v <- o .: "VTablePtr"
      MetaVTablePtr <$> parseJSON v
    String "Unknown" -> pure MetaUnknown
    _ -> fail "Unknown variant"

