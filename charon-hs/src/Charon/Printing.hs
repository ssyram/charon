{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-|
Module: Charon.Printing
Description: Printing mechanism for the generated Haskell AST

This module provides a printing mechanism for the Charon AST, mirroring the
functionality in charon/src/pretty/fmt_with_ctx.rs. It uses Builder for
efficient string construction.
-}
module Charon.Printing
  ( -- * Context
    PrintingCtx(..)
  , emptyCtx
  , pushGenerics
  , setLocals
  , increaseIndent
  , indent
    -- * Typeclass
  , BuildWithCtx(..)
  , printWithCtx
    -- * Crate printing
  , printCrate
  , printDecls
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Generated_GAst
import qualified Generated_GAst as G
import Generated_Types
import qualified Generated_Types as T
import Generated_Values
import Generated_Expressions
import qualified Generated_Expressions as E
import Generated_Krate
import qualified Generated_Krate as K
import qualified Generated_Meta as M
import qualified Generated_LlbcAst as L
import qualified Generated_UllbcAst as U
import Data.List (intercalate)

-- | Tab increment for indentation (4 spaces)
tabIncr :: Text
tabIncr = "    "

-- | The context (analogous to AstFormatter trait in Rust)
data PrintingCtx = PrintingCtx
  { translated :: Maybe TranslatedCrate
  , generics :: [GenericParams]  -- binding stack as pure list
  , locals :: Maybe Locals
  , indentLevel :: Int
  } deriving (Show, Eq)

-- | Create an empty printing context
emptyCtx :: PrintingCtx
emptyCtx = PrintingCtx
  { translated = Nothing
  , generics = []
  , locals = Nothing
  , indentLevel = 0
  }

-- | Push generics onto the binding stack
pushGenerics :: GenericParams -> PrintingCtx -> PrintingCtx
pushGenerics g ctx = ctx { generics = g : generics ctx }

-- | Set the locals in the context
setLocals :: Locals -> PrintingCtx -> PrintingCtx
setLocals l ctx = ctx { locals = Just l }

-- | Increase indentation level
increaseIndent :: PrintingCtx -> PrintingCtx
increaseIndent ctx = ctx { indentLevel = indentLevel ctx + 1 }

-- | Get the current indentation string
indent :: PrintingCtx -> Text
indent ctx = T.replicate (indentLevel ctx) tabIncr

-- | Single typeclass for formatting with context
class BuildWithCtx a where
  buildWithCtx :: PrintingCtx -> a -> Builder

-- | Convert to a lazy Text string using the context
printWithCtx :: BuildWithCtx a => PrintingCtx -> a -> String
printWithCtx ctx = TL.unpack . B.toLazyText . buildWithCtx ctx

-- Helper function to build from Text
fromText :: Text -> Builder
fromText = B.fromText

-- Helper function to build from String
fromString :: String -> Builder
fromString = B.fromString

-- Helper function to punctuate a list with a separator
punctuate :: Builder -> [Builder] -> [Builder]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate sep (x:xs) = (x <> sep) : punctuate sep xs

-- Helper function to check if GenericParams has predicates
hasPredicates :: T.GenericParams -> Bool
hasPredicates (T.GenericParams _ _ _ (M.Vector traitClauses) regionsOutlive typesOutlive (M.Vector traitTypeConstraints)) =
  not (null traitClauses && null regionsOutlive && null typesOutlive && null traitTypeConstraints)

--------------------
-- Basic instances
--------------------

instance BuildWithCtx Text where
  buildWithCtx _ = fromText

instance BuildWithCtx String where
  buildWithCtx _ = fromString

instance BuildWithCtx Int where
  buildWithCtx _ = B.decimal

instance BuildWithCtx Integer where
  buildWithCtx _ = B.decimal

instance BuildWithCtx Bool where
  buildWithCtx _ True = "true"
  buildWithCtx _ False = "false"

-- Maybe instance
instance BuildWithCtx a => BuildWithCtx (Maybe a) where
  buildWithCtx ctx (Just x) = buildWithCtx ctx x
  buildWithCtx _ Nothing = ""

-- List instance - renders comma-separated
instance BuildWithCtx a => BuildWithCtx [a] where
  buildWithCtx ctx xs = mconcat $ punctuate (fromText ", ") (map (buildWithCtx ctx) xs)

--------------------
-- AST instances
--------------------

-- AbortKind
instance BuildWithCtx AbortKind where
  buildWithCtx ctx (Panic maybeName) = 
    "panic" <> case maybeName of
      Nothing -> ""
      Just n -> "(" <> buildWithCtx ctx n <> ")"
  buildWithCtx _ UndefinedBehavior = "undefined_behavior"
  buildWithCtx _ UnwindTerminate = "unwind_terminate"

-- Name
instance BuildWithCtx Name where
  buildWithCtx ctx (Name items) = 
    -- Reset generics to avoid names being displayed differently depending on the current binding level
    let ctx' = ctx { generics = [] }
    in mconcat $ punctuate (fromText "::") (map (buildWithCtx ctx') items)

-- PathElem
instance BuildWithCtx PathElem where
  buildWithCtx _ (PeIdent name (Disambiguator d)) = 
    fromString name <> if d == 0 then "" else "#" <> B.decimal d
  buildWithCtx _ (PeImpl _) = "{impl}"
  buildWithCtx _ (PeInstantiated _) = "{instantiated}"

-- Disambiguator
instance BuildWithCtx Disambiguator where
  buildWithCtx _ (Disambiguator d) = B.decimal d

-- OverflowMode
instance BuildWithCtx OverflowMode where
  buildWithCtx _ OPanic = "panic"
  buildWithCtx _ OWrap = "wrap"
  buildWithCtx _ Oub = "ub"

-- Binop
instance BuildWithCtx Binop where
  buildWithCtx _ BitXor = "^"
  buildWithCtx _ BitAnd = "&"
  buildWithCtx _ BitOr = "|"
  buildWithCtx _ Eq = "=="
  buildWithCtx _ Lt = "<"
  buildWithCtx _ Le = "<="
  buildWithCtx _ Ne = "!="
  buildWithCtx _ Ge = ">="
  buildWithCtx _ Gt = ">"
  buildWithCtx ctx (Add mode) = buildWithCtx ctx mode <> ".+"
  buildWithCtx ctx (Sub mode) = buildWithCtx ctx mode <> ".-"
  buildWithCtx ctx (Mul mode) = buildWithCtx ctx mode <> ".*"
  buildWithCtx ctx (Div mode) = buildWithCtx ctx mode <> "./"
  buildWithCtx ctx (Rem mode) = buildWithCtx ctx mode <> ".%"
  buildWithCtx _ AddChecked = "checked.+"
  buildWithCtx _ SubChecked = "checked.-"
  buildWithCtx _ MulChecked = "checked.*"
  buildWithCtx ctx (Shl mode) = buildWithCtx ctx mode <> ".<<"
  buildWithCtx ctx (Shr mode) = buildWithCtx ctx mode <> ".>>"
  buildWithCtx _ Cmp = "cmp"
  buildWithCtx _ Offset = "offset"

-- Unop
instance BuildWithCtx Unop where
  buildWithCtx _ Not = "~"
  buildWithCtx ctx (Neg mode) = buildWithCtx ctx mode <> ".-"
  buildWithCtx ctx (Cast ck) = buildWithCtx ctx ck

-- CastKind
instance BuildWithCtx CastKind where
  buildWithCtx ctx (CastScalar src tgt) = 
    "cast<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastRawPtr src tgt) =
    "cast<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastFnPtr src tgt) =
    "cast<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastUnsize src tgt _meta) =
    "unsize_cast<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastTransmute src tgt) =
    "transmute<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"
  buildWithCtx ctx (CastConcretize src tgt) =
    "concretize<" <> buildWithCtx ctx src <> ", " <> buildWithCtx ctx tgt <> ">"

-- IntTy
instance BuildWithCtx IntTy where
  buildWithCtx _ Isize = "isize"
  buildWithCtx _ I8 = "i8"
  buildWithCtx _ I16 = "i16"
  buildWithCtx _ I32 = "i32"
  buildWithCtx _ I64 = "i64"
  buildWithCtx _ I128 = "i128"

-- UIntTy  
instance BuildWithCtx UIntTy where
  buildWithCtx _ Usize = "usize"
  buildWithCtx _ U8 = "u8"
  buildWithCtx _ U16 = "u16"
  buildWithCtx _ U32 = "u32"
  buildWithCtx _ U64 = "u64"
  buildWithCtx _ U128 = "u128"

-- FloatType
instance BuildWithCtx FloatType where
  buildWithCtx _ F16 = "f16"
  buildWithCtx _ F32 = "f32"
  buildWithCtx _ F64 = "f64"
  buildWithCtx _ F128 = "f128"

-- RefKind
instance BuildWithCtx RefKind where
  buildWithCtx _ RShared = "shared"
  buildWithCtx _ RMut = "mut"

-- BorrowKind
instance BuildWithCtx BorrowKind where
  buildWithCtx _ BShared = "Shared"
  buildWithCtx _ BMut = "Mut"
  buildWithCtx _ BTwoPhaseMut = "TwoPhaseMut"
  buildWithCtx _ BShallow = "Shallow"
  buildWithCtx _ BUniqueImmutable = "UniqueImmutable"

-- LocalId
instance BuildWithCtx LocalId where
  buildWithCtx ctx (LocalId i) =
    case locals ctx of
      Just (Locals _ _locs) -> 
        -- Try to get the variable name from the locals vector
        -- For now, just show the ID
        "@" <> B.decimal i
      Nothing -> "@" <> B.decimal i

-- FieldId
instance BuildWithCtx FieldId where
  buildWithCtx _ (FieldId i) = B.decimal i

-- VariantId
instance BuildWithCtx VariantId where
  buildWithCtx _ (VariantId i) = B.decimal i

-- Helper to look up item name from crate context
lookupItemName :: PrintingCtx -> ItemId -> Maybe Name
lookupItemName ctx itemId =
  case translated ctx of
    Just crate -> 
      -- Look up in short names first
      case lookup itemId (translatedcrateShortNames crate) of
        Just name -> Just name
        Nothing -> lookup itemId (translatedcrateItemNames crate)
    Nothing -> Nothing
  where
    lookup key items = case filter (\kv -> kvpairKey kv == key) items of
      (kv:_) -> Just (kvpairValue kv)
      [] -> Nothing

-- TypeDeclId
instance BuildWithCtx TypeDeclId where
  buildWithCtx ctx tid =
    case lookupItemName ctx (IdType tid) of
      Just name -> buildWithCtx ctx name
      Nothing -> "@Type" <> B.decimal (typedeclidRaw tid)

-- FunDeclId
instance BuildWithCtx FunDeclId where
  buildWithCtx ctx fid =
    case lookupItemName ctx (IdFun fid) of
      Just name -> buildWithCtx ctx name
      Nothing -> "@Fun" <> B.decimal (fundeclidRaw fid)

-- GlobalDeclId
instance BuildWithCtx GlobalDeclId where
  buildWithCtx ctx gid =
    case lookupItemName ctx (IdGlobal gid) of
      Just name -> buildWithCtx ctx name
      Nothing -> "@Global" <> B.decimal (globaldeclidRaw gid)

-- TraitDeclId
instance BuildWithCtx TraitDeclId where
  buildWithCtx ctx tid =
    case lookupItemName ctx (IdTraitDecl tid) of
      Just name -> buildWithCtx ctx name
      Nothing -> "@Trait" <> B.decimal (traitdeclidRaw tid)

-- TraitImplId
instance BuildWithCtx TraitImplId where
  buildWithCtx ctx tid =
    case lookupItemName ctx (IdTraitImpl tid) of
      Just name -> buildWithCtx ctx name
      Nothing -> "@TraitImpl" <> B.decimal (traitimplidRaw tid)

-- Assertion
instance BuildWithCtx Assertion where
  buildWithCtx ctx (Assertion cond expected _onFailure) =
    "assert(" <> buildWithCtx ctx cond <> " == " <> buildWithCtx ctx expected <> ")"

-- Call
instance BuildWithCtx Call where
  buildWithCtx ctx (Call func args dest) =
    buildWithCtx ctx dest <> " := " <> buildWithCtx ctx func <> "(" <>
    buildWithCtx ctx args <> ")"

-- BorrowKind already defined above, skipping duplicate

-- BuiltinFunId
instance BuildWithCtx BuiltinFunId where
  buildWithCtx _ BoxNew = "BoxNew"
  buildWithCtx _ ArrayToSliceShared = "ArrayToSliceShared"
  buildWithCtx _ ArrayToSliceMut = "ArrayToSliceMut"
  buildWithCtx _ ArrayRepeat = "ArrayRepeat"
  buildWithCtx _ctx (Index (BuiltinIndexOp isArray mutability isRange)) =
    let ty = if isArray then "Array" else "Slice"
        op = if isRange then "SubSlice" else "Index"
        mut = case mutability of
                RShared -> "Shared"
                RMut -> "Mut"
    in fromString (ty ++ op ++ mut)
  buildWithCtx _ (PtrFromParts RShared) = "PtrFromPartsShared"
  buildWithCtx _ (PtrFromParts RMut) = "PtrFromPartsMut"

-- Add more instances as needed...

-- Region
instance BuildWithCtx Region where
  buildWithCtx ctx (RVar var) = buildWithCtx ctx var
  buildWithCtx _ RStatic = "'static"

-- DeBruijnVar
instance BuildWithCtx a => BuildWithCtx (DeBruijnVar a) where
  buildWithCtx ctx (Bound dbid varid) = 
    "Bound(" <> buildWithCtx ctx dbid <> ", " <> buildWithCtx ctx varid <> ")"
  buildWithCtx ctx (Free varid) = buildWithCtx ctx varid

-- DeBruijnId
instance BuildWithCtx DeBruijnId where
  buildWithCtx _ (DeBruijnId i) = B.decimal i

-- RegionId
instance BuildWithCtx RegionId where
  buildWithCtx _ (RegionId i) = B.decimal i

-- TypeVarId
instance BuildWithCtx TypeVarId where
  buildWithCtx _ (TypeVarId i) = B.decimal i

-- ConstGenericVarId
instance BuildWithCtx ConstGenericVarId where
  buildWithCtx _ (ConstGenericVarId i) = B.decimal i

-- TraitClauseId
instance BuildWithCtx TraitClauseId where
  buildWithCtx _ (TraitClauseId i) = "@TraitClause" <> B.decimal i

-- NullOp
instance BuildWithCtx Nullop where
  buildWithCtx _ SizeOf = "size_of"
  buildWithCtx _ AlignOf = "align_of"
  buildWithCtx _ (OffsetOf _) = "offset_of(?)"
  buildWithCtx _ UbChecks = "ub_checks"

-- ConstantExpr
instance BuildWithCtx ConstantExpr where
  buildWithCtx ctx (ConstantExpr kind _ty) = buildWithCtx ctx kind

-- TraitItemName
instance BuildWithCtx TraitItemName where
  buildWithCtx _ (TraitItemName t) = fromText t

-- ConstantExprKind
instance BuildWithCtx ConstantExprKind where
  buildWithCtx ctx (CLiteral lit) = buildWithCtx ctx lit
  buildWithCtx ctx (CVar cg) = buildWithCtx ctx cg
  buildWithCtx ctx (CTraitConst traitRef name) = 
    buildWithCtx ctx traitRef <> "::" <> buildWithCtx ctx name
  buildWithCtx ctx (CFnPtr fnPtr) = buildWithCtx ctx fnPtr
  buildWithCtx _ (CRawMemory bytes) = "RawMemory(" <> fromString (show bytes) <> ")"
  buildWithCtx _ (COpaque s) = "Opaque(" <> fromString s <> ")"

-- Literal
instance BuildWithCtx Literal where
  buildWithCtx ctx (VScalar sv) = buildWithCtx ctx sv
  buildWithCtx ctx (VFloat fv) = buildWithCtx ctx fv
  buildWithCtx _ (VBool b) = if b then "true" else "false"
  buildWithCtx _ (VChar c) = fromString (show c)
  buildWithCtx _ (VStr s) = "\"" <> fromString (escapeString s) <> "\""
    where
      escapeString = concatMap escapeChar
      escapeChar '\\' = "\\\\"
      escapeChar '\n' = "\\n"
      escapeChar '\r' = "\\r"
      escapeChar '\t' = "\\t"
      escapeChar c = [c]
  buildWithCtx _ (VByteStr bs) = fromString (show bs)

-- FloatValue
instance BuildWithCtx FloatValue where
  buildWithCtx _ (FloatValue val fty) = 
    fromString val <> " : " <> buildWithCtx emptyCtx fty

-- ScalarValue
instance BuildWithCtx ScalarValue where
  buildWithCtx _ (UnsignedScalar uty iv) = 
    fromString (show iv) <> " : " <> buildWithCtx emptyCtx uty
  buildWithCtx _ (SignedScalar ity iv) = 
    fromString (show iv) <> " : " <> buildWithCtx emptyCtx ity

-- IntegerType
instance BuildWithCtx IntegerType where
  buildWithCtx _ (Signed ity) = buildWithCtx emptyCtx ity
  buildWithCtx _ (Unsigned uty) = buildWithCtx emptyCtx uty

-- LiteralType  
instance BuildWithCtx LiteralType where
  buildWithCtx _ (TInt ity) = buildWithCtx emptyCtx ity
  buildWithCtx _ (TuInt uty) = buildWithCtx emptyCtx uty
  buildWithCtx _ (TFloat fty) = buildWithCtx emptyCtx fty
  buildWithCtx _ TChar = "char"
  buildWithCtx _ TBool = "bool"

-- FnOperand
instance BuildWithCtx FnOperand where
  buildWithCtx ctx (FnOpRegular fnPtr) = buildWithCtx ctx fnPtr
  buildWithCtx ctx (FnOpMove place) = "(move " <> buildWithCtx ctx place <> ")"

-- FnPtr
instance BuildWithCtx FnPtr where
  buildWithCtx ctx (FnPtr kind generics) =
    buildWithCtx ctx kind <> buildWithCtx ctx generics

-- FnPtrKind
instance BuildWithCtx FnPtrKind where
  buildWithCtx ctx (FunId funId) = buildWithCtx ctx funId
  buildWithCtx ctx (Generated_Types.TraitMethod traitRef methodName _) =
    buildWithCtx ctx traitRef <> "::" <> buildWithCtx ctx methodName

-- FunId
instance BuildWithCtx FunId where
  buildWithCtx ctx (FRegular fid) = buildWithCtx ctx fid
  buildWithCtx _ (FBuiltin bid) = "@" <> buildWithCtx emptyCtx bid

-- Operand
instance BuildWithCtx Operand where
  buildWithCtx ctx (Copy place) = "copy (" <> buildWithCtx ctx place <> ")"
  buildWithCtx ctx (Move place) = "move (" <> buildWithCtx ctx place <> ")"
  buildWithCtx ctx (Constant ce) = "const (" <> buildWithCtx ctx ce <> ")"

-- Place
instance BuildWithCtx Place where
  buildWithCtx ctx (Place kind _ty) = buildPlaceKind ctx kind
    where
      buildPlaceKind ctx' (PlaceLocal lid) = buildWithCtx ctx' lid
      buildPlaceKind ctx' (PlaceGlobal gref) = buildWithCtx ctx' gref
      buildPlaceKind ctx' (PlaceProjection subplace proj) =
        let base = buildPlaceKind ctx' (placeKind subplace)
        in buildProjection ctx' base proj

      buildProjection ctx' base Deref = "*(" <> base <> ")"
      buildProjection ctx' base (Generated_Expressions.Field projKind fid) =
        case projKind of
          ProjAdt _tid Nothing -> "(" <> base <> ")." <> buildWithCtx ctx' fid
          ProjAdt _tid (Just _vid) -> "(" <> base <> ")." <> buildWithCtx ctx' fid
          ProjTuple _ -> "(" <> base <> ")." <> buildWithCtx ctx' fid
      buildProjection _ base PtrMetadata = base <> ".metadata"
      buildProjection ctx' base (ProjIndex offset fromEnd) = 
        if fromEnd
        then "(" <> base <> ")[-" <> buildWithCtx ctx' offset <> "]"
        else "(" <> base <> ")[" <> buildWithCtx ctx' offset <> "]"
      buildProjection ctx' base (Subslice from to fromEnd) =
        if fromEnd
        then "(" <> base <> ")[" <> buildWithCtx ctx' from <> "..-" <> buildWithCtx ctx' to <> "]"
        else "(" <> base <> ")[" <> buildWithCtx ctx' from <> ".." <> buildWithCtx ctx' to <> "]"

-- PlaceKind
instance BuildWithCtx PlaceKind where
  buildWithCtx ctx (PlaceLocal lid) = buildWithCtx ctx lid
  buildWithCtx ctx (PlaceGlobal gref) = buildWithCtx ctx gref

-- GlobalDeclRef
instance BuildWithCtx GlobalDeclRef where
  buildWithCtx ctx (GlobalDeclRef gid generics) =
    buildWithCtx ctx gid <> buildWithCtx ctx generics

-- GenericArgs
instance BuildWithCtx GenericArgs where
  buildWithCtx ctx ga@(GenericArgs (M.Vector regions) (M.Vector types) (M.Vector constGenerics) (M.Vector traitRefs)) =
    let hasExplicits = not (null regions && null types && null constGenerics)
        hasImplicits = not (null traitRefs)
        explicits = if hasExplicits
          then "<" <> mconcat (punctuate ", " (
                 map (buildWithCtx ctx) regions ++
                 map (buildWithCtx ctx) types ++
                 map (buildWithCtx ctx) constGenerics
               )) <> ">"
          else ""
        implicits = if hasImplicits
          then "[" <> mconcat (punctuate ", " (map (buildWithCtx ctx) traitRefs)) <> "]"
          else ""
    in explicits <> implicits

-- TraitRef
instance BuildWithCtx TraitRef where
  buildWithCtx ctx (TraitRef kind traitDeclRef) =
    case kind of
      BuiltinOrAuto kindId types -> 
        -- Format as {built_in impl TraitName<...> for Type<...>}
        let traitImpl = "{built_in impl " <> buildWithCtx ctx traitDeclRef <> "}"
        in traitImpl
      _ -> buildWithCtx ctx kind <> buildWithCtx ctx traitDeclRef

-- TraitRefKind
-- TraitRefKind
instance BuildWithCtx TraitRefKind where
  buildWithCtx ctx (Generated_Types.TraitImpl implRef) = buildWithCtx ctx implRef
  buildWithCtx ctx (Clause var) = buildWithCtx ctx var
  buildWithCtx ctx (ParentClause tr cid) = 
    buildWithCtx ctx tr <> "::parent_clause" <> B.decimal (traitclauseidRaw cid)
  buildWithCtx ctx (ItemClause tr name cid) =
    "(" <> buildWithCtx ctx tr <> "::" <> fromText (traitItemNameText name) <> "::[@TraitClause" <> B.decimal (traitclauseidRaw cid) <> "])"
  buildWithCtx _ Self = "Self"
  buildWithCtx _ (BuiltinOrAuto _ _ _) = ""  -- Handled in TraitRef instance

-- RegionBinder for function pointer types (Vec<Ty>, Ty) - MUST come before generic instance
instance BuildWithCtx (RegionBinder ([Ty], Ty)) where
  buildWithCtx ctx (RegionBinder (M.Vector regions) (inputs, output)) =
    let ctx' = pushBoundRegions ctx regions
    in "fn" <>
       (if null regions then "" else "<" <> mconcat (punctuate ", " (map (buildWithCtx ctx') regions)) <> ">") <>
       "(" <> mconcat (punctuate ", " (map (buildWithCtx ctx') inputs)) <> ")" <>
       (if isUnitType output then "" else " -> " <> buildWithCtx ctx' output)
    where
      isUnitType (TAdt (TypeDeclRef TTuple (GenericArgs (M.Vector []) (M.Vector []) (M.Vector []) (M.Vector [])))) = True
      isUnitType _ = False
      pushBoundRegions c _ = c  -- Note: In Rust, this pushes bound regions to context; we don't track them

-- RegionBinder - generic instance for other types
instance BuildWithCtx a => BuildWithCtx (RegionBinder a) where
  buildWithCtx ctx (RegionBinder (M.Vector regions) value) =
    if null regions
    then buildWithCtx ctx value
    else "for<" <> mconcat (punctuate ", " (map (buildWithCtx ctx) regions)) <> "> " <> buildWithCtx ctx value

-- TraitImplRef
instance BuildWithCtx TraitImplRef where
  buildWithCtx ctx (TraitImplRef tid generics) =
    buildWithCtx ctx tid <> buildWithCtx ctx generics

-- TraitDeclRef
instance BuildWithCtx TraitDeclRef where
  buildWithCtx ctx (TraitDeclRef tid generics) =
    buildWithCtx ctx tid <> buildWithCtx ctx generics

-- ConstGeneric
instance BuildWithCtx ConstGeneric where
  buildWithCtx ctx (CgVar varid) = buildWithCtx ctx varid
  buildWithCtx _ (CgValue lit) = buildWithCtx emptyCtx lit
  buildWithCtx ctx (CgGlobal gid) = buildWithCtx ctx gid

-- DynPredicate
instance BuildWithCtx DynPredicate where
  buildWithCtx ctx (DynPredicate binder) = buildWithCtx ctx binder

-- Binder Ty (for dyn traits)
instance BuildWithCtx (Binder Ty) where
  buildWithCtx ctx (Binder binderParams ty) =
    let T.GenericParams _ (M.Vector typeParams) _ (M.Vector traitClauses) _ _ _ = binderParams
        predicates = if null traitClauses
          then ""
          else mconcat (punctuate " + " (map (buildWithCtx ctx) traitClauses))
    in if null traitClauses
      then buildWithCtx ctx ty
      else predicates

-- Ty (Type) - This is complex, start with basic structure
instance BuildWithCtx Ty where
  buildWithCtx ctx (TAdt tdeclRef) = buildWithCtx ctx tdeclRef
  buildWithCtx ctx (TVar tvid) = buildWithCtx ctx tvid
  buildWithCtx _ (TLiteral lt) = buildWithCtx emptyCtx lt
  buildWithCtx _ TNever = "!"
  buildWithCtx ctx (TRef region ty refKind) =
    "&" <> buildWithCtx ctx region <> " " <> buildWithCtx ctx refKind <> " " <> buildWithCtx ctx ty
  buildWithCtx ctx (TRawPtr ty refKind) =
    "*" <> buildWithCtx ctx refKind <> " " <> buildWithCtx ctx ty
  buildWithCtx ctx (TTraitType traitRef tyName) =
    buildWithCtx ctx traitRef <> "::" <> buildWithCtx ctx tyName
  buildWithCtx ctx (TDynTrait predicate) =
    "(dyn " <> buildWithCtx ctx predicate <> ")"
  buildWithCtx ctx (TFnPtr regionBinder) = buildWithCtx ctx regionBinder
  buildWithCtx ctx (TFnDef binder) =
    let ctx' = ctx { generics = [] }  -- Reset generics for function types
    in buildWithCtx ctx' binder
  buildWithCtx ctx (TPtrMetadata ty) = "PtrMetadata<" <> buildWithCtx ctx ty <> ">"
  buildWithCtx _ (TError msg) = "Error(" <> fromString msg <> ")"

-- TypeDeclRef
instance BuildWithCtx TypeDeclRef where
  buildWithCtx ctx (TypeDeclRef tid generics) =
    buildWithCtx ctx tid <> buildWithCtx ctx generics

-- TypeId
instance BuildWithCtx TypeId where
  buildWithCtx ctx (TAdtId tdid) = buildWithCtx ctx tdid
  buildWithCtx _ TTuple = "()"
  buildWithCtx ctx (TBuiltin bt) = buildWithCtx ctx bt

-- BuiltinTy
instance BuildWithCtx BuiltinTy where
  buildWithCtx _ TBox = "Box"
  buildWithCtx _ TArray = "Array"
  buildWithCtx _ TSlice = "Slice"
  buildWithCtx _ TStr = "str"

-- FunSig
instance BuildWithCtx FunSig where
  buildWithCtx ctx (FunSig isUnsafe _generics inputs output) =
    let unsafe = if isUnsafe then "unsafe " else ""
    in unsafe <> "fn(" <> mconcat (punctuate ", " (map (buildWithCtx ctx) inputs)) <> ")" <>
       (if isUnitType output then "" else " -> " <> buildWithCtx ctx output)
    where
      isUnitType (TAdt (TypeDeclRef TTuple (GenericArgs (M.Vector []) (M.Vector []) (M.Vector []) (M.Vector [])))) = True
      isUnitType _ = False

-- Rvalue
instance BuildWithCtx E.Rvalue where
  buildWithCtx ctx (E.Use op) = buildWithCtx ctx op
  buildWithCtx ctx (E.RvRef place bkind metadata) =
    let borrow = case bkind of
          BShared -> "&"
          BMut -> "&mut "
          BTwoPhaseMut -> "&two-phase-mut "
          BUniqueImmutable -> "&uniq "
          BShallow -> "&shallow "
    in borrow <> buildWithCtx ctx place <>
       (if isUnitOperand metadata then "" else " with_metadata(" <> buildWithCtx ctx metadata <> ")")
    where
      isUnitOperand (Constant (ConstantExpr _ (TAdt (TypeDeclRef TTuple (GenericArgs (M.Vector []) (M.Vector []) (M.Vector []) (M.Vector [])))))) = True
      isUnitOperand _ = False
  buildWithCtx ctx (E.RawPtr place refKind metadata) =
    let ptrKind = case refKind of
          RShared -> "&raw const "
          RMut -> "&raw mut "
    in ptrKind <> buildWithCtx ctx place <>
       (if isUnitOperand metadata then "" else " with_metadata(" <> buildWithCtx ctx metadata <> ")")
    where
      isUnitOperand (Constant (ConstantExpr _ (TAdt (TypeDeclRef TTuple (GenericArgs (M.Vector []) (M.Vector []) (M.Vector []) (M.Vector [])))))) = True
      isUnitOperand _ = False
  buildWithCtx ctx (E.BinaryOp binop op1 op2) =
    buildWithCtx ctx op1 <> " " <> buildWithCtx ctx binop <> " " <> buildWithCtx ctx op2
  buildWithCtx ctx (E.UnaryOp unop op) =
    buildWithCtx ctx unop <> " " <> buildWithCtx ctx op
  buildWithCtx ctx (E.NullaryOp nullop ty) =
    buildWithCtx ctx nullop <> "<" <> buildWithCtx ctx ty <> ">"
  buildWithCtx ctx (E.Discriminant place) =
    "@discriminant(" <> buildWithCtx ctx place <> ")"
  buildWithCtx ctx (E.Aggregate aggKind ops) =
    buildWithCtx ctx aggKind <> " { " <> mconcat (punctuate ", " (map (buildWithCtx ctx) ops)) <> " }"
  buildWithCtx ctx (E.Len place ty _maybeConst) =
    "len(" <> buildWithCtx ctx place <> ")"
  buildWithCtx ctx (E.Repeat op ty cg) =
    "[" <> buildWithCtx ctx op <> "; " <> buildWithCtx ctx cg <> "]"
  buildWithCtx ctx (E.ShallowInitBox op ty) =
    "shallow_init_box(" <> buildWithCtx ctx op <> ")"

-- AggregateKind
instance BuildWithCtx E.AggregateKind where
  buildWithCtx ctx (E.AggregatedAdt tdeclRef variantId _maybeFieldId) =
    buildWithCtx ctx tdeclRef <>
    (case variantId of
       Nothing -> ""
       Just vid -> "::" <> buildWithCtx ctx vid)
  buildWithCtx ctx (E.AggregatedArray ty cg) =
    "Array"
  buildWithCtx ctx (E.AggregatedRawPtr ty refKind) =
    "RawPtr"

-- Statement
instance BuildWithCtx L.Statement where
  buildWithCtx ctx (L.Statement _span _id kind commentsBefore) =
    mconcat (map (\c -> fromText (indent ctx) <> "// " <> fromString c <> "\n") commentsBefore) <>
    buildWithCtx ctx kind

-- StatementKind  
instance BuildWithCtx L.StatementKind where
  buildWithCtx ctx (L.Assign place rvalue) =
    fromText (indent ctx) <> buildWithCtx ctx place <> " := " <> buildWithCtx ctx rvalue
  buildWithCtx ctx (L.SetDiscriminant place variantId) =
    fromText (indent ctx) <> "@discriminant(" <> buildWithCtx ctx place <> ") := " <> buildWithCtx ctx variantId
  buildWithCtx ctx (L.CopyNonOverlapping (G.CopyNonOverlapping src dst count)) =
    fromText (indent ctx) <> "copy_nonoverlapping(" <> 
    buildWithCtx ctx src <> ", " <> buildWithCtx ctx dst <> ", " <> buildWithCtx ctx count <> ")"
  buildWithCtx ctx (L.StorageLive lid) =
    fromText (indent ctx) <> "storage_live(" <> buildWithCtx ctx lid <> ")"
  buildWithCtx ctx (L.StorageDead lid) =
    fromText (indent ctx) <> "storage_dead(" <> buildWithCtx ctx lid <> ")"
  buildWithCtx ctx (L.Deinit place) =
    fromText (indent ctx) <> "deinit(" <> buildWithCtx ctx place <> ")"
  buildWithCtx ctx (L.Drop place traitRef) =
    fromText (indent ctx) <> "drop[" <> buildWithCtx ctx traitRef <> "] " <> buildWithCtx ctx place
  buildWithCtx ctx (L.Assert assertion) =
    fromText (indent ctx) <> buildWithCtx ctx assertion
  buildWithCtx ctx (L.Call call) =
    fromText (indent ctx) <> buildWithCtx ctx call
  buildWithCtx ctx (L.Abort abortKind) =
    fromText (indent ctx) <> "abort(" <> buildWithCtx ctx abortKind <> ")"
  buildWithCtx ctx L.Return =
    fromText (indent ctx) <> "return"
  buildWithCtx ctx (L.Break n) =
    fromText (indent ctx) <> "break " <> B.decimal n
  buildWithCtx ctx (L.Continue n) =
    fromText (indent ctx) <> "continue " <> B.decimal n
  buildWithCtx ctx L.Nop =
    fromText (indent ctx) <> "nop"
  buildWithCtx ctx (L.Switch switch) =
    buildWithCtx ctx switch
  buildWithCtx ctx (L.Loop block) =
    fromText (indent ctx) <> "loop " <> buildWithCtx ctx block
  buildWithCtx ctx (L.Error msg) =
    fromText (indent ctx) <> "@Error(" <> fromString msg <> ")"

-- Switch
instance BuildWithCtx L.Switch where
  buildWithCtx ctx (L.If cond thenBlock elseBlock) =
    fromText (indent ctx) <> "if " <> buildWithCtx ctx cond <> " {\n" <>
    buildWithCtx ctx thenBlock <>
    fromText (indent ctx) <> "} else {\n" <>
    buildWithCtx ctx elseBlock <>
    fromText (indent ctx) <> "}"
  buildWithCtx ctx (L.SwitchInt op _litTy branches defaultBlock) =
    fromText (indent ctx) <> "match " <> buildWithCtx ctx op <> " {\n" <>
    mconcat (map (\(lits, blk) ->
      fromText (indent ctx) <> "  " <>
      mconcat (punctuate " | " (map (buildWithCtx ctx) lits)) <> " => " <>
      buildWithCtx ctx blk) branches) <>
    fromText (indent ctx) <> "  _ => " <> buildWithCtx ctx defaultBlock <>
    fromText (indent ctx) <> "}"
  buildWithCtx ctx (L.Match place branches _maybeDefaultBlock) =
    fromText (indent ctx) <> "match " <> buildWithCtx ctx place <> " {\n" <>
    mconcat (map (\(variantIds, blk) ->
      fromText (indent ctx) <> "  variant " <>
      mconcat (punctuate " | " (map (buildWithCtx ctx) variantIds)) <> " => " <>
      buildWithCtx ctx blk) branches) <>
    fromText (indent ctx) <> "}"

-- Block
instance BuildWithCtx L.Block where
  buildWithCtx ctx (L.Block _span statements) =
    mconcat (map (\stmt -> buildWithCtx ctx stmt <> "\n") statements)

-- FunDecl
instance BuildWithCtx K.FunDecl where
  buildWithCtx ctx (K.FunDecl defId itemMeta signature _src _isGlobalInit body) =
    let ctxWithGenerics = pushGenerics (funsigGenerics signature) ctx
    in "// Full name: " <> buildWithCtx ctx (T.itemmetaName itemMeta) <> "\n" <>
       formatLangItem itemMeta <>
       (if M.attrinfoPublic (T.itemmetaAttrInfo itemMeta) then "pub " else "") <>
       (if funsigIsUnsafe signature then "unsafe " else "") <>
       "fn " <> buildWithCtx ctx (T.itemmetaName itemMeta) <>
       formatGenericParams ctxWithGenerics signature <>
       formatArgs ctxWithGenerics signature <>
       formatReturnType ctxWithGenerics signature <>
       formatPredicates ctxWithGenerics signature <>
       "\n" <>
       buildWithCtx (increaseIndent ctx) body
    where
      formatLangItem im = case T.itemmetaLangItem im of
        Just li -> "#[lang_item(\"" <> fromString li <> "\")]\n"
        Nothing -> ""
      formatGenericParams c sig = 
        let (params, _) = formatGenericParamsWithClauses c (funsigGenerics sig)
        in params
      formatArgs c (FunSig _ _ inputs _) =
        "(" <> mconcat (punctuate ", " (zipWith formatArg [1..] inputs)) <> ")"
        where
          formatArg i ty = "@" <> B.decimal i <> ": " <> buildWithCtx c ty
      formatReturnType c (FunSig _ _ _ output) =
        if isUnit output then "" else " -> " <> buildWithCtx c output
        where
          isUnit (TAdt (TypeDeclRef TTuple (GenericArgs (M.Vector []) (M.Vector []) (M.Vector []) (M.Vector [])))) = True
          isUnit _ = False
      formatPredicates c sig =
        let (_, clauses) = formatGenericParamsWithClauses c (funsigGenerics sig)
        in clauses

-- Body (from Krate)
instance BuildWithCtx K.Body where
  buildWithCtx ctx (K.Structured gexprBody) =
    buildWithCtx ctx gexprBody
  buildWithCtx ctx (K.Unstructured gexprBody) =
    buildWithCtx ctx gexprBody
  buildWithCtx _ K.TraitMethodWithoutDefault = "= <method_without_default_body>"
  buildWithCtx _ K.Opaque = "= <opaque>"
  buildWithCtx _ K.Missing = "= <missing>"
  buildWithCtx ctx (K.Error err) = "= <error: " <> fromString (show err) <> ">"

-- GexprBody for Block
instance BuildWithCtx (G.GexprBody L.Block) where
  buildWithCtx ctx (G.GexprBody _span locals body) =
    "{\n" <>
    formatLocals (increaseIndent ctx) locals <>
    buildWithCtx (increaseIndent ctx) body <>
    fromText (indent ctx) <> "}"
    where
      formatLocals c (G.Locals argCount (M.Vector localsList)) =
        mconcat (map (formatLocal c argCount) localsList)
      formatLocal c argCount (G.Local idx name ty) =
        fromText (indent c) <> "let " <> formatLocalName name idx <> ": " <>
        buildWithCtx c ty <> "; // " <> formatComment argCount idx <> "\n"
      formatLocalName (Just n) idx = fromString n <> "@" <> B.decimal (E.localidRaw idx)
      formatLocalName Nothing idx = "@" <> B.decimal (E.localidRaw idx)
      formatComment argCount idx
        | E.localidRaw idx == 0 = "return"
        | E.localidRaw idx < argCount = "arg #" <> B.decimal (E.localidRaw idx)
        | otherwise = "local"

-- GexprBody for unstructured (Vector of BlockId Block)
instance BuildWithCtx (G.GexprBody (M.Vector U.BlockId U.Block)) where
  buildWithCtx ctx (G.GexprBody _span locals blocks) =
    let tab = indent ctx
        ctx' = increaseIndent ctx
    in "\n" <> fromText tab <>
       "{\n" <>
       formatLocals ctx' locals <>
       formatBlocks ctx' blocks <>
       fromText tab <> "}"
    where
      formatLocals c (G.Locals argCount (M.Vector localsList)) =
        mconcat (map (formatLocal c argCount) localsList)
      formatLocal c argCount (G.Local idx name ty) =
        fromText (indent c) <> "let " <> formatLocalName name idx <> ": " <>
        buildWithCtx c ty <> "; // " <> formatComment argCount idx <> "\n"
      formatLocalName (Just n) idx = fromString n <> "@" <> B.decimal (E.localidRaw idx)
      formatLocalName Nothing idx = "@" <> B.decimal (E.localidRaw idx)
      formatComment argCount idx
        | E.localidRaw idx == 0 = "return"
        | E.localidRaw idx < argCount = "arg #" <> B.decimal (E.localidRaw idx)
        | otherwise = "local"
      formatBlocks c (M.Vector blockList) =
        mconcat (zipWith (formatBlock c) [0..] blockList)
      formatBlock c bid block =
        "\n" <> fromText (indent c) <> "bb" <> B.decimal bid <> ": {\n" <>
        buildWithCtx (increaseIndent c) block <>
        "\n" <> fromText (indent c) <> "}\n"

-- ItemMeta name formatting
instance BuildWithCtx [T.PathElem] where
  buildWithCtx ctx elems =
    mconcat (punctuate "::" (map (buildWithCtx ctx) elems))

-- Unstructured Statement
instance BuildWithCtx U.Statement where
  buildWithCtx ctx (U.Statement _span kind commentsBefore) =
    mconcat (map (\c -> fromText (indent ctx) <> "// " <> fromString c <> "\n") commentsBefore) <>
    buildWithCtx ctx kind

-- Unstructured StatementKind
instance BuildWithCtx U.StatementKind where
  buildWithCtx ctx (U.Assign place rvalue) =
    fromText (indent ctx) <> buildWithCtx ctx place <> " := " <> buildWithCtx ctx rvalue
  buildWithCtx ctx (U.SetDiscriminant place variantId) =
    fromText (indent ctx) <> "@discriminant(" <> buildWithCtx ctx place <> ") := " <> buildWithCtx ctx variantId
  buildWithCtx ctx (U.CopyNonOverlapping (G.CopyNonOverlapping src dst count)) =
    fromText (indent ctx) <> "copy_nonoverlapping(" <> 
    buildWithCtx ctx src <> ", " <> buildWithCtx ctx dst <> ", " <> buildWithCtx ctx count <> ")"
  buildWithCtx ctx (U.StorageLive lid) =
    fromText (indent ctx) <> "storage_live(" <> buildWithCtx ctx lid <> ")"
  buildWithCtx ctx (U.StorageDead lid) =
    fromText (indent ctx) <> "storage_dead(" <> buildWithCtx ctx lid <> ")"
  buildWithCtx ctx (U.Deinit place) =
    fromText (indent ctx) <> "deinit(" <> buildWithCtx ctx place <> ")"
  buildWithCtx ctx (U.Assert assertion) =
    fromText (indent ctx) <> buildWithCtx ctx assertion
  buildWithCtx ctx U.Nop =
    fromText (indent ctx) <> "nop"

-- Unstructured Block
instance BuildWithCtx U.Block where
  buildWithCtx ctx (U.Block stmts terminator) =
    mconcat (map (buildWithCtx ctx) stmts) <>
    fromText (indent ctx) <> buildWithCtx ctx terminator

-- Unstructured Terminator  
instance BuildWithCtx U.Terminator where
  buildWithCtx ctx (U.Terminator _span kind commentsBefore) =
    mconcat (map (\c -> fromText (indent ctx) <> "// " <> fromString c <> "\n") commentsBefore) <>
    fromText (indent ctx) <> buildWithCtx ctx kind

-- Unstructured TerminatorKind
instance BuildWithCtx U.TerminatorKind where
  buildWithCtx _ (U.Goto target) =
    "goto bb" <> B.decimal (U.blockidRaw target)
  buildWithCtx ctx (U.Switch discr (U.If trueBlock falseBlock)) =
    "if " <> buildWithCtx ctx discr <> " -> bb" <> B.decimal (U.blockidRaw trueBlock) <>
    " else -> bb" <> B.decimal (U.blockidRaw falseBlock)
  buildWithCtx ctx (U.Switch discr targets) =
    "switch " <> buildWithCtx ctx discr <> " -> " <> buildWithCtx ctx targets
  buildWithCtx ctx (U.Call call target onUnwind) =
    buildWithCtx ctx call <> " -> bb" <> B.decimal (U.blockidRaw target) <> 
    " (unwind: bb" <> B.decimal (U.blockidRaw onUnwind) <> ")"
  buildWithCtx ctx (U.Drop place tref target onUnwind) =
    "drop[" <> buildWithCtx ctx tref <> "] " <> buildWithCtx ctx place <> 
    " -> bb" <> B.decimal (U.blockidRaw target) <> " (unwind: bb" <> B.decimal (U.blockidRaw onUnwind) <> ")"
  buildWithCtx ctx (U.Abort kind) =
    buildWithCtx ctx kind
  buildWithCtx _ U.Return =
    "return"
  buildWithCtx _ U.UnwindResume =
    "unwind_continue"

-- Switch formatting for unstructured terminators (only SwitchInt now, If is handled above)
instance BuildWithCtx U.Switch where
  buildWithCtx _ (U.If trueBlock falseBlock) =
    -- This shouldn't be reached since If is handled specially in TerminatorKind
    "bb" <> B.decimal (U.blockidRaw trueBlock) <> " else -> bb" <> B.decimal (U.blockidRaw falseBlock)
  buildWithCtx ctx (U.SwitchInt _ty maps otherwise) =
    let formatMap (lit, bid) = buildWithCtx ctx lit <> ": bb" <> B.decimal (U.blockidRaw bid)
        allMaps = map formatMap maps ++ ["otherwise: bb" <> B.decimal (U.blockidRaw otherwise)]
    in mconcat (punctuate ", " allMaps)

-- TypeDecl
instance BuildWithCtx T.TypeDecl where
  buildWithCtx ctx (T.TypeDecl defId itemMeta generics _src kind _layout _ptrMetadata _repr) =
    let ctxWithGenerics = pushGenerics generics ctx
        keyword = case kind of
          T.Struct _ -> "struct"
          T.Union _ -> "union"
          T.Enum _ -> "enum"
          T.Alias _ -> "type"
          T.Opaque -> "opaque type"
          T.TDeclError _ -> "opaque type"
        (params, clauses) = formatGenericParamsWithClauses ctxWithGenerics generics
        nlOrSpace = if hasPredicates generics then "\n" else " "
    in "// Full name: " <> buildWithCtx ctx (T.itemmetaName itemMeta) <> "\n" <>
       formatLangItem itemMeta <>
       (if M.attrinfoPublic (T.itemmetaAttrInfo itemMeta) then "pub " else "") <>
       fromString keyword <> " " <> buildWithCtx ctx (T.itemmetaName itemMeta) <>
       params <> clauses <>
       formatKind ctxWithGenerics nlOrSpace kind
    where
      formatLangItem im = case T.itemmetaLangItem im of
        Just li -> "#[lang_item(\"" <> fromString li <> "\")]\n"
        Nothing -> ""
      hasPredicates (T.GenericParams _ _ _ (M.Vector traitClauses) regionsOutlive typesOutlive (M.Vector traitTypeConstraints)) =
        not (null traitClauses && null regionsOutlive && null typesOutlive && null traitTypeConstraints)
      formatKind c nlOrSpace (T.Struct (M.Vector fields)) =
        nlOrSpace <> "{\n" <>
        mconcat (map (\f -> "  " <> buildWithCtx c f <> ",\n") fields) <>
        "}"
      formatKind c nlOrSpace (T.Union (M.Vector fields)) =
        nlOrSpace <> "{\n" <>
        mconcat (map (\f -> "  " <> buildWithCtx c f <> ",\n") fields) <>
        "}"
      formatKind c nlOrSpace (T.Enum (M.Vector variants)) =
        nlOrSpace <> "{\n" <>
        mconcat (map (\v -> "  " <> buildWithCtx c v <> ",\n") variants) <>
        "}"
      formatKind c nlOrSpace (T.Alias ty) = " = " <> buildWithCtx c ty
      formatKind c nlOrSpace T.Opaque = ""
      formatKind c nlOrSpace (T.TDeclError msg) = " = ERROR(" <> fromString msg <> ")"

-- Field
instance BuildWithCtx T.Field where
  buildWithCtx ctx (T.Field _span _attrInfo name ty) =
    case name of
      Just n -> fromString n <> ": " <> buildWithCtx ctx ty
      Nothing -> buildWithCtx ctx ty

-- Variant
instance BuildWithCtx T.Variant where
  buildWithCtx ctx (T.Variant _span _attrInfo name (M.Vector fields) _discr) =
    fromString name <>
    (case fields of
       [] -> ""
       _ -> "(" <> mconcat (punctuate ", " (map (buildWithCtx ctx) fields)) <> ")")

-- GlobalDecl
instance BuildWithCtx G.GlobalDecl where
  buildWithCtx ctx (G.GlobalDecl defId itemMeta generics ty _src globalKind init) =
    let ctxWithGenerics = pushGenerics generics ctx
        keyword = case globalKind of
          G.Static -> "static"
          G.NamedConst -> "const"
          G.AnonConst -> "const"
        (params, clauses) = formatGenericParamsWithClauses ctxWithGenerics generics
    in "// Full name: " <> buildWithCtx ctx (T.itemmetaName itemMeta) <> "\n" <>
       (if M.attrinfoPublic (T.itemmetaAttrInfo itemMeta) then "pub " else "") <>
       fromString keyword <> " " <> buildWithCtx ctx (T.itemmetaName itemMeta) <>
       params <> ": " <> buildWithCtx ctxWithGenerics ty <>
       clauses <>
       (if hasPredicates generics then "\n" else " ") <>
       "= " <> buildWithCtx ctx init <> "()"
    where
      hasPredicates (T.GenericParams _ _ _ (M.Vector traitClauses) regionsOutlive typesOutlive (M.Vector traitTypeConstraints)) =
        not (null traitClauses && null regionsOutlive && null typesOutlive && null traitTypeConstraints)

-- TraitDecl
instance BuildWithCtx G.TraitDecl where
  buildWithCtx ctx (G.TraitDecl defId itemMeta generics impliedClauses consts types methods vtable) =
    let ctxWithGenerics = pushGenerics generics ctx
        (params, clauses) = formatGenericParamsWithClauses ctxWithGenerics generics
    in "// Full name: " <> buildWithCtx ctx (T.itemmetaName itemMeta) <> "\n" <>
       formatLangItem itemMeta <>
       (if M.attrinfoPublic (T.itemmetaAttrInfo itemMeta) then "pub " else "") <>
       "trait " <> buildWithCtx ctx (T.itemmetaName itemMeta) <>
       params <> clauses <>
       formatBody ctxWithGenerics impliedClauses consts types methods vtable
    where
      formatLangItem im = case T.itemmetaLangItem im of
        Just li -> "#[lang_item(\"" <> fromString li <> "\")]\n"
        Nothing -> ""
      formatBody c (M.Vector implClauses) consts types methods vtable =
        let anyItem = not (null implClauses) || not (null consts) || not (null types) || not (null methods)
        in if anyItem
           then "\n{\n" <>
                mconcat (zipWith formatImpliedClause [0..] implClauses) <>
                mconcat (map formatConst consts) <>
                mconcat (map formatType types) <>
                mconcat (map formatMethod methods) <>
                formatVtable vtable <>
                "}"
           else ""
      formatImpliedClause idx clause =
        "    parent_clause" <> B.decimal idx <> " : " <> buildWithCtx ctx clause <> "\n"
      formatConst (G.TraitAssocConst name ty _default) =
        let T.TraitItemName n = name
        in "    const " <> fromText n <> " : " <> buildWithCtx ctx ty <> "\n"
      formatType (T.Binder binderParams assocTy) =
        let T.TraitItemName name = G.traitassoctyName assocTy
            T.GenericParams (M.Vector regions) _ _ _ _ _ _ = binderParams
            params = if null regions then "" else "<" <> mconcat (punctuate ", " (map (buildWithCtx ctx) regions)) <> ">"
        in "    type " <> fromText name <> params <> "\n"
      formatMethod (T.Binder _regions method) =
        let T.TraitItemName name = G.traitmethodName method
        in "    fn " <> fromText name <> " = " <>
           buildWithCtx ctx (G.traitmethodItem method) <> "\n"
      formatVtable Nothing = "    non-dyn-compatible\n"
      formatVtable (Just ref) = "    vtable: " <> buildWithCtx ctx ref <> "\n"

-- TraitMethod
instance BuildWithCtx G.TraitMethod where
  buildWithCtx ctx (G.TraitMethod (T.TraitItemName name) item) =
    fromText name

-- FunDeclRef  
instance BuildWithCtx T.FunDeclRef where
  buildWithCtx ctx (T.FunDeclRef funId generics) =
    buildWithCtx ctx funId <> buildWithCtx ctx generics

-- TraitParam
instance BuildWithCtx T.TraitParam where
  buildWithCtx ctx (T.TraitParam clauseId _span traitRef) =
    buildWithCtx ctx traitRef

-- GenericParams formatting helpers
formatGenericParamsWithClauses :: PrintingCtx -> T.GenericParams -> (Builder, Builder)
formatGenericParamsWithClauses ctx gp =
  let params = if hasExplicits gp
                 then "<" <> formatParams ctx gp <> ">"
                 else ""
      clauses = if hasPredicates gp
                  then "\n" <> fromText (indent ctx) <> "where" <> formatClauses ctx gp
                  else ""
  in (params, clauses)
  where
    hasExplicits (T.GenericParams (M.Vector regions) (M.Vector types) (M.Vector constGens) _ _ _ _) =
      not (null regions && null types && null constGens)
    formatParams c (T.GenericParams (M.Vector regions) (M.Vector types) (M.Vector constGens) _ _ _ _) =
      mconcat (punctuate ", " (map (buildWithCtx c) regions ++ map (buildWithCtx c) types ++ map (buildWithCtx c) constGens))
    formatClauses c (T.GenericParams _ _ _ (M.Vector traitClauses) regionsOutlive typesOutlive (M.Vector traitTypeConstraints)) =
      mconcat (map (\tc -> "\n" <> fromText (indent c) <> "    " <> buildWithCtx c tc <> ",") traitClauses)

-- RegionParam
instance BuildWithCtx T.RegionParam where
  buildWithCtx ctx (T.RegionParam regionId maybeName) =
    case maybeName of
      Just name -> fromString name
      Nothing -> buildWithCtx ctx regionId

-- TypeParam
instance BuildWithCtx T.TypeParam where
  buildWithCtx ctx (T.TypeParam typeVarId name) =
    fromString name

-- ConstGenericParam
instance BuildWithCtx T.ConstGenericParam where
  buildWithCtx ctx (T.ConstGenericParam constVarId name ty) =
    "const " <> fromString name <> ": " <> buildWithCtx ctx ty

-- GenericParams
instance BuildWithCtx T.GenericParams where
  buildWithCtx ctx gp =
    let (params, clauses) = formatGenericParamsWithClauses ctx gp
    in params <> clauses

-- TraitImpl
instance BuildWithCtx G.TraitImpl where
  buildWithCtx ctx (G.TraitImpl defId itemMeta implTrait generics impliedTraitRefs consts types methods vtable) =
    let ctxWithGenerics = pushGenerics generics ctx
        (params, clauses) = formatGenericParamsWithClauses ctxWithGenerics generics
    in "// Full name: " <> buildWithCtx ctx (T.itemmetaName itemMeta) <> "\n" <>
       (if M.attrinfoPublic (T.itemmetaAttrInfo itemMeta) then "pub " else "") <>
       "impl" <> params <> " " <> buildWithCtx ctxWithGenerics implTrait <>
       clauses <>
       (if hasPredicates generics then "\n" else " ") <>
       formatBody ctxWithGenerics impliedTraitRefs consts types methods vtable
    where
      formatBody c (M.Vector impliedRefs) constsList typesList methodsList mvtable =
        "{\n" <>
        formatImpliedRefs c impliedRefs <>
        formatConsts c constsList <>
        formatTypes c typesList <>
        formatMethods c methodsList <>
        formatVtable c mvtable <>
        "}"
      formatImpliedRefs c [] = ""
      formatImpliedRefs c refs =
        mconcat (zipWith (\i r -> "    parent_clause" <> B.decimal i <> " = " <> buildWithCtx c r <> "\n") [0::Int ..] refs)
      formatConsts c [] = ""
      formatConsts c constsList =
        mconcat (map (\(T.TraitItemName name, ref) -> "    const " <> fromText name <> " = " <> buildWithCtx c ref <> "\n") constsList)
      formatTypes c [] = ""
      formatTypes c typesList =
        mconcat (map (\(T.TraitItemName name, binder) -> "    type " <> fromText name <> " = " <> buildWithCtx c binder <> "\n") typesList)
      formatMethods c [] = ""
      formatMethods c methodsList =
        mconcat (map (\(T.TraitItemName name, binder) -> "    fn " <> fromText name <> " = " <> buildWithCtx c binder <> "\n") methodsList)
      formatVtable c Nothing = ""
      formatVtable c (Just ref) = "    vtable = " <> buildWithCtx c ref <> "\n"

-- Binder for TraitAssocTyImpl
instance BuildWithCtx (T.Binder T.TraitAssocTyImpl) where
  buildWithCtx ctx (T.Binder params value) =
    -- Push the binder's generic params onto the context
    let ctxWithParams = pushGenerics params ctx
    in buildWithCtx ctxWithParams value

-- TraitAssocTyImpl
instance BuildWithCtx T.TraitAssocTyImpl where
  buildWithCtx ctx (T.TraitAssocTyImpl ty) = buildWithCtx ctx ty

-- Binder for FunDeclRef
instance BuildWithCtx (T.Binder T.FunDeclRef) where
  buildWithCtx ctx (T.Binder params value) =
    -- For FunDeclRef, we don't push params since it's just a reference
    buildWithCtx ctx value

-- | Print an entire TranslatedCrate to String with header
printCrate :: TranslatedCrate -> String
printCrate crate =
  let ctx = emptyCtx { translated = Just crate }
      header = "# Final LLBC before serialization:\n\n"
      decls = printDecls ctx crate
  in header ++ decls

-- | Print all declarations in a crate
printDecls :: PrintingCtx -> TranslatedCrate -> String
printDecls ctx crate =
  let types = map (\decl -> printWithCtx ctx decl ++ "\n") (M.vectorToList $ translatedcrateTypeDecls crate)
      funs = map (\decl -> printWithCtx ctx decl ++ "\n") (M.vectorToList $ translatedcrateFunDecls crate)
      globals = map (\decl -> printWithCtx ctx decl ++ "\n") (M.vectorToList $ translatedcrateGlobalDecls crate)
      traitDecls = map (\decl -> printWithCtx ctx decl ++ "\n") (M.vectorToList $ translatedcrateTraitDecls crate)
      traitImpls = map (\decl -> printWithCtx ctx decl ++ "\n") (M.vectorToList $ translatedcrateTraitImpls crate)
  in concat (types ++ funs ++ globals ++ traitDecls ++ traitImpls)
