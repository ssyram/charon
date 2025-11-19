{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_GAstOfJson.hs`
by hand. Edit `templates/GAstOfJson.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_GAstOfJson where

import Data.Aeson
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import qualified Generated_Meta as M
import Generated_Meta
import Generated_Values
import qualified Generated_Types as T
import Generated_Types
import qualified Generated_Expressions as E
import Generated_Expressions
-- Import specific types from Generated_GAst that we need for TranslatedCrate
-- TraitImpl, TraitMethod, Local, Call, Assertion, CopyNonOverlapping will be qualified with G.
import Generated_GAst (Preset(..), TargetInfo(..), TraitAssocConst(..), TraitAssocTy(..), TraitDecl(..), MirLevel(..), MonomorphizeMut(..), GlobalKind(..), Locals(..), GDeclarationGroup(..), GexprBody(..), GlobalDecl(..), CliOptions(..), DeclarationGroup(..), FnOperand(..), FunSig(..))
import qualified Generated_GAst as G

-- Vector newtype is defined in Generated_Meta to avoid circular dependencies

-- Manual instance for TranslatedCrate - simplified version that parses the key fields
-- Full deserialization would require FunDecl and Body instances which have complex dependencies
data TranslatedCrate = TranslatedCrate
  { translatedCrateCrate_name :: String
  , translatedCrateType_decls :: Vector TypeDeclId TypeDecl
  , translatedCrateGlobal_decls :: Vector GlobalDeclId GlobalDecl
  , translatedCrateTrait_decls :: Vector TraitDeclId TraitDecl
  , translatedCrateTrait_impls :: Vector TraitImplId G.TraitImpl
  }
  deriving (Show, Eq, Ord)

-- Wrapper type for the top-level LLBC file structure
data LlbcFile = LlbcFile
  { llbcfileCharon_version :: String
  , llbcfileTranslated :: TranslatedCrate
  }
  deriving (Show, Eq, Ord)

instance FromJSON TranslatedCrate where
  parseJSON = withObject "TranslatedCrate" $ \o -> do
    crateName <- o .: "crate_name"
    typeDecls <- o .: "type_decls"
    globalDecls <- o .: "global_decls"
    traitDecls <- o .: "trait_decls"
    traitImpls <- o .: "trait_impls"
    -- We skip fields that we can't deserialize yet (options, target_information, fun_decls, etc.)
    pure $ TranslatedCrate crateName typeDecls globalDecls traitDecls traitImpls

instance FromJSON LlbcFile where
  parseJSON = withObject "LlbcFile" $ \o -> do
    charonVersion <- o .: "charon_version"
    translated <- o .: "translated"
    pure $ LlbcFile charonVersion translated

instance FromJSON AbortKind where
  parseJSON v = case v of
    Object o | H.lookup "Panic" o /= Nothing -> do
      v <- o .: "Panic"
      T.Panic <$> parseJSON v
    String "UndefinedBehavior" -> pure T.UndefinedBehavior
    String "UnwindTerminate" -> pure T.UnwindTerminate
    _ -> fail "Unknown variant"


instance FromJSON AggregateKind where
  parseJSON v = case v of
    Object o | H.lookup "Adt" o /= Nothing -> do
      withArray "AggregatedAdt" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (E.AggregatedAdt v0 v1 v2)) =<< o .: "Adt"
    Object o | H.lookup "Array" o /= Nothing -> do
      withArray "AggregatedArray" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.AggregatedArray v0 v1)) =<< o .: "Array"
    Object o | H.lookup "RawPtr" o /= Nothing -> do
      withArray "AggregatedRawPtr" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.AggregatedRawPtr v0 v1)) =<< o .: "RawPtr"
    _ -> fail "Unknown variant"


instance FromJSON AlignmentModifier where
  parseJSON v = case v of
    Object o | H.lookup "Align" o /= Nothing -> do
      v <- o .: "Align"
      T.Align <$> parseJSON v
    Object o | H.lookup "Pack" o /= Nothing -> do
      v <- o .: "Pack"
      T.Pack <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON Assertion where
  parseJSON = withObject "Assertion" $ \o -> do
    assertionCond <- o .: "cond"
    assertionExpected <- o .: "expected"
    assertionOnFailure <- o .: "on_failure"
    pure (Assertion assertionCond assertionExpected assertionOnFailure)


instance FromJSON AttrInfo where
  parseJSON = withObject "AttrInfo" $ \o -> do
    attrinfoAttributes <- o .: "attributes"
    attrinfoInline <- o .: "inline"
    attrinfoRename <- o .: "rename"
    attrinfoPublic <- o .: "public"
    pure (AttrInfo attrinfoAttributes attrinfoInline attrinfoRename attrinfoPublic)


instance FromJSON Attribute where
  parseJSON v = case v of
    String "Opaque" -> pure M.AttrOpaque
    Object o | H.lookup "Rename" o /= Nothing -> do
      v <- o .: "Rename"
      M.AttrRename <$> parseJSON v
    Object o | H.lookup "VariantsPrefix" o /= Nothing -> do
      v <- o .: "VariantsPrefix"
      M.AttrVariantsPrefix <$> parseJSON v
    Object o | H.lookup "VariantsSuffix" o /= Nothing -> do
      v <- o .: "VariantsSuffix"
      M.AttrVariantsSuffix <$> parseJSON v
    Object o | H.lookup "DocComment" o /= Nothing -> do
      v <- o .: "DocComment"
      M.AttrDocComment <$> parseJSON v
    Object o | H.lookup "Unknown" o /= Nothing -> do
      v <- o .: "Unknown"
      M.AttrUnknown <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON Binop where
  parseJSON v = case v of
    String "BitXor" -> pure E.BitXor
    String "BitAnd" -> pure E.BitAnd
    String "BitOr" -> pure E.BitOr
    String "Eq" -> pure E.Eq
    String "Lt" -> pure E.Lt
    String "Le" -> pure E.Le
    String "Ne" -> pure E.Ne
    String "Ge" -> pure E.Ge
    String "Gt" -> pure E.Gt
    Object o | H.lookup "Add" o /= Nothing -> do
      v <- o .: "Add"
      E.Add <$> parseJSON v
    Object o | H.lookup "Sub" o /= Nothing -> do
      v <- o .: "Sub"
      E.Sub <$> parseJSON v
    Object o | H.lookup "Mul" o /= Nothing -> do
      v <- o .: "Mul"
      E.Mul <$> parseJSON v
    Object o | H.lookup "Div" o /= Nothing -> do
      v <- o .: "Div"
      E.Div <$> parseJSON v
    Object o | H.lookup "Rem" o /= Nothing -> do
      v <- o .: "Rem"
      E.Rem <$> parseJSON v
    String "AddChecked" -> pure E.AddChecked
    String "SubChecked" -> pure E.SubChecked
    String "MulChecked" -> pure E.MulChecked
    Object o | H.lookup "Shl" o /= Nothing -> do
      v <- o .: "Shl"
      E.Shl <$> parseJSON v
    Object o | H.lookup "Shr" o /= Nothing -> do
      v <- o .: "Shr"
      E.Shr <$> parseJSON v
    String "Offset" -> pure E.Offset
    String "Cmp" -> pure E.Cmp
    _ -> fail "Unknown variant"


instance (FromJSON a0) => FromJSON (Binder a0) where
  parseJSON = withObject "Binder" $ \o -> do
    binderBinderParams <- o .: "params"
    binderBinderValue <- o .: "skip_binder"
    pure (Binder binderBinderParams binderBinderValue)


instance FromJSON BinderKind where
  parseJSON v = case v of
    Object o | H.lookup "TraitType" o /= Nothing -> do
      withArray "BkTraitType" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (T.BkTraitType v0 v1)) =<< o .: "TraitType"
    Object o | H.lookup "TraitMethod" o /= Nothing -> do
      withArray "BkTraitMethod" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (T.BkTraitMethod v0 v1)) =<< o .: "TraitMethod"
    String "InherentImplBlock" -> pure T.BkInherentImplBlock
    String "Dyn" -> pure T.BkDyn
    String "Other" -> pure T.BkOther
    _ -> fail "Unknown variant"


instance FromJSON BorrowKind where
  parseJSON v = case v of
    String "Shared" -> pure E.BShared
    String "Mut" -> pure E.BMut
    String "TwoPhaseMut" -> pure E.BTwoPhaseMut
    String "Shallow" -> pure E.BShallow
    String "UniqueImmutable" -> pure E.BUniqueImmutable
    _ -> fail "Unknown variant"


instance FromJSON BuiltinFunId where
  parseJSON v = case v of
    String "BoxNew" -> pure T.BoxNew
    String "ArrayToSliceShared" -> pure T.ArrayToSliceShared
    String "ArrayToSliceMut" -> pure T.ArrayToSliceMut
    String "ArrayRepeat" -> pure T.ArrayRepeat
    Object o | H.lookup "Index" o /= Nothing -> do
      v <- o .: "Index"
      T.Index <$> parseJSON v
    Object o | H.lookup "PtrFromParts" o /= Nothing -> do
      v <- o .: "PtrFromParts"
      T.PtrFromParts <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON BuiltinImplData where
  parseJSON v = case v of
    String "Sized" -> pure T.BuiltinSized
    String "MetaSized" -> pure T.BuiltinMetaSized
    String "Tuple" -> pure T.BuiltinTuple
    String "Send" -> pure T.BuiltinSend
    String "Sync" -> pure T.BuiltinSync
    String "Pointee" -> pure T.BuiltinPointee
    String "DiscriminantKind" -> pure T.BuiltinDiscriminantKind
    String "Unpin" -> pure T.BuiltinUnpin
    String "Freeze" -> pure T.BuiltinFreeze
    String "NoopDestruct" -> pure T.BuiltinNoopDestruct
    String "UntrackedDestruct" -> pure T.BuiltinUntrackedDestruct
    String "Fn" -> pure T.BuiltinFn
    String "FnMut" -> pure T.BuiltinFnMut
    String "FnOnce" -> pure T.BuiltinFnOnce
    String "Copy" -> pure T.BuiltinCopy
    String "Clone" -> pure T.BuiltinClone
    _ -> fail "Unknown variant"


instance FromJSON BuiltinIndexOp where
  parseJSON = withObject "BuiltinIndexOp" $ \o -> do
    builtinindexopIsArray <- o .: "is_array"
    builtinindexopMutability <- o .: "mutability"
    builtinindexopIsRange <- o .: "is_range"
    pure (BuiltinIndexOp builtinindexopIsArray builtinindexopMutability builtinindexopIsRange)


instance FromJSON BuiltinTy where
  parseJSON v = case v of
    String "Box" -> pure T.TBox
    String "Array" -> pure T.TArray
    String "Slice" -> pure T.TSlice
    String "Str" -> pure T.TStr
    _ -> fail "Unknown variant"


instance FromJSON Call where
  parseJSON = withObject "Call" $ \o -> do
    callFunc <- o .: "func"
    callArgs <- o .: "args"
    callDest <- o .: "dest"
    pure (Call callFunc callArgs callDest)


instance FromJSON CastKind where
  parseJSON v = case v of
    Object o | H.lookup "Scalar" o /= Nothing -> do
      withArray "CastScalar" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.CastScalar v0 v1)) =<< o .: "Scalar"
    Object o | H.lookup "RawPtr" o /= Nothing -> do
      withArray "CastRawPtr" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.CastRawPtr v0 v1)) =<< o .: "RawPtr"
    Object o | H.lookup "FnPtr" o /= Nothing -> do
      withArray "CastFnPtr" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.CastFnPtr v0 v1)) =<< o .: "FnPtr"
    Object o | H.lookup "Unsize" o /= Nothing -> do
      withArray "CastUnsize" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (E.CastUnsize v0 v1 v2)) =<< o .: "Unsize"
    Object o | H.lookup "Transmute" o /= Nothing -> do
      withArray "CastTransmute" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.CastTransmute v0 v1)) =<< o .: "Transmute"
    Object o | H.lookup "Concretize" o /= Nothing -> do
      withArray "CastConcretize" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.CastConcretize v0 v1)) =<< o .: "Concretize"
    _ -> fail "Unknown variant"


instance FromJSON CliOptions where
  parseJSON = withObject "CliOptions" $ \o -> do
    clioptionsUllbc <- o .: "ullbc"
    clioptionsLib <- o .: "lib"
    clioptionsBin <- o .: "bin"
    clioptionsMirPromoted <- o .: "mir_promoted"
    clioptionsMirOptimized <- o .: "mir_optimized"
    clioptionsMir <- o .: "mir"
    clioptionsInputFile <- o .: "input_file"
    clioptionsReadLlbc <- o .: "read_llbc"
    clioptionsDestDir <- o .: "dest_dir"
    clioptionsDestFile <- o .: "dest_file"
    clioptionsUsePolonius <- o .: "use_polonius"
    clioptionsSkipBorrowck <- o .: "skip_borrowck"
    clioptionsMonomorphize <- o .: "monomorphize"
    clioptionsMonomorphizeMut <- o .: "monomorphize_mut"
    clioptionsExtractOpaqueBodies <- o .: "extract_opaque_bodies"
    clioptionsTranslateAllMethods <- o .: "translate_all_methods"
    clioptionsIncluded <- o .: "include"
    clioptionsOpaque <- o .: "opaque"
    clioptionsExclude <- o .: "exclude"
    clioptionsRemoveAssociatedTypes <- o .: "remove_associated_types"
    clioptionsHideMarkerTraits <- o .: "hide_marker_traits"
    clioptionsRemoveAdtClauses <- o .: "remove_adt_clauses"
    clioptionsHideAllocator <- o .: "hide_allocator"
    clioptionsRemoveUnusedSelfClauses <- o .: "remove_unused_self_clauses"
    clioptionsAddDropBounds <- o .: "add_drop_bounds"
    clioptionsStartFrom <- o .: "start_from"
    clioptionsNoCargo <- o .: "no_cargo"
    clioptionsRustcArgs <- o .: "rustc_args"
    clioptionsCargoArgs <- o .: "cargo_args"
    clioptionsAbortOnError <- o .: "abort_on_error"
    clioptionsErrorOnWarnings <- o .: "error_on_warnings"
    clioptionsNoSerialize <- o .: "no_serialize"
    clioptionsPrintOriginalUllbc <- o .: "print_original_ullbc"
    clioptionsPrintUllbc <- o .: "print_ullbc"
    clioptionsPrintBuiltLlbc <- o .: "print_built_llbc"
    clioptionsPrintLlbc <- o .: "print_llbc"
    clioptionsNoMergeGotoChains <- o .: "no_merge_goto_chains"
    clioptionsNoOpsToFunctionCalls <- o .: "no_ops_to_function_calls"
    clioptionsRawBoxes <- o .: "raw_boxes"
    clioptionsPreset <- o .: "preset"
    pure (CliOptions clioptionsUllbc clioptionsLib clioptionsBin clioptionsMirPromoted clioptionsMirOptimized clioptionsMir clioptionsInputFile clioptionsReadLlbc clioptionsDestDir clioptionsDestFile clioptionsUsePolonius clioptionsSkipBorrowck clioptionsMonomorphize clioptionsMonomorphizeMut clioptionsExtractOpaqueBodies clioptionsTranslateAllMethods clioptionsIncluded clioptionsOpaque clioptionsExclude clioptionsRemoveAssociatedTypes clioptionsHideMarkerTraits clioptionsRemoveAdtClauses clioptionsHideAllocator clioptionsRemoveUnusedSelfClauses clioptionsAddDropBounds clioptionsStartFrom clioptionsNoCargo clioptionsRustcArgs clioptionsCargoArgs clioptionsAbortOnError clioptionsErrorOnWarnings clioptionsNoSerialize clioptionsPrintOriginalUllbc clioptionsPrintUllbc clioptionsPrintBuiltLlbc clioptionsPrintLlbc clioptionsNoMergeGotoChains clioptionsNoOpsToFunctionCalls clioptionsRawBoxes clioptionsPreset)


instance FromJSON ClosureInfo where
  parseJSON = withObject "ClosureInfo" $ \o -> do
    closureinfoKind <- o .: "kind"
    closureinfoFnOnceImpl <- o .: "fn_once_impl"
    closureinfoFnMutImpl <- o .: "fn_mut_impl"
    closureinfoFnImpl <- o .: "fn_impl"
    closureinfoSignature <- o .: "signature"
    pure (ClosureInfo closureinfoKind closureinfoFnOnceImpl closureinfoFnMutImpl closureinfoFnImpl closureinfoSignature)


instance FromJSON ClosureKind where
  parseJSON v = case v of
    String "Fn" -> pure T.Fn
    String "FnMut" -> pure T.FnMut
    String "FnOnce" -> pure T.FnOnce
    _ -> fail "Unknown variant"


instance FromJSON ConstGeneric where
  parseJSON v = case v of
    Object o | H.lookup "Global" o /= Nothing -> do
      v <- o .: "Global"
      T.CgGlobal <$> parseJSON v
    Object o | H.lookup "Var" o /= Nothing -> do
      v <- o .: "Var"
      T.CgVar <$> parseJSON v
    Object o | H.lookup "Value" o /= Nothing -> do
      v <- o .: "Value"
      T.CgValue <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON ConstGenericParam where
  parseJSON = withObject "ConstGenericParam" $ \o -> do
    constgenericparamIndex <- o .: "index"
    constgenericparamName <- o .: "name"
    constgenericparamTy <- o .: "ty"
    pure (ConstGenericParam constgenericparamIndex constgenericparamName constgenericparamTy)


instance FromJSON ConstGenericVarId where
  parseJSON = fmap ConstGenericVarId . parseJSON


instance FromJSON ConstantExpr where
  parseJSON = withObject "ConstantExpr" $ \o -> do
    constantexprKind <- o .: "kind"
    constantexprTy <- o .: "ty"
    pure (ConstantExpr constantexprKind constantexprTy)


instance FromJSON ConstantExprKind where
  parseJSON v = case v of
    Object o | H.lookup "Literal" o /= Nothing -> do
      v <- o .: "Literal"
      E.CLiteral <$> parseJSON v
    Object o | H.lookup "TraitConst" o /= Nothing -> do
      withArray "CTraitConst" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.CTraitConst v0 v1)) =<< o .: "TraitConst"
    Object o | H.lookup "Var" o /= Nothing -> do
      v <- o .: "Var"
      E.CVar <$> parseJSON v
    Object o | H.lookup "FnPtr" o /= Nothing -> do
      v <- o .: "FnPtr"
      E.CFnPtr <$> parseJSON v
    Object o | H.lookup "RawMemory" o /= Nothing -> do
      v <- o .: "RawMemory"
      E.CRawMemory <$> parseJSON v
    Object o | H.lookup "Opaque" o /= Nothing -> do
      v <- o .: "Opaque"
      E.COpaque <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON CopyNonOverlapping where
  parseJSON = withObject "CopyNonOverlapping" $ \o -> do
    copynonoverlappingSrc <- o .: "src"
    copynonoverlappingDst <- o .: "dst"
    copynonoverlappingCount <- o .: "count"
    pure (CopyNonOverlapping copynonoverlappingSrc copynonoverlappingDst copynonoverlappingCount)


instance FromJSON DeBruijnId where
  parseJSON = fmap DeBruijnId . parseJSON


instance (FromJSON a0) => FromJSON (DeBruijnVar a0) where
  parseJSON v = case v of
    Object o | H.lookup "Bound" o /= Nothing -> do
      withArray "Bound" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (T.Bound v0 v1)) =<< o .: "Bound"
    Object o | H.lookup "Free" o /= Nothing -> do
      v <- o .: "Free"
      T.Free <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON DeclarationGroup where
  parseJSON v = case v of
    Object o | H.lookup "Type" o /= Nothing -> do
      v <- o .: "Type"
      TypeGroup <$> parseJSON v
    Object o | H.lookup "Fun" o /= Nothing -> do
      v <- o .: "Fun"
      FunGroup <$> parseJSON v
    Object o | H.lookup "Global" o /= Nothing -> do
      v <- o .: "Global"
      GlobalGroup <$> parseJSON v
    Object o | H.lookup "TraitDecl" o /= Nothing -> do
      v <- o .: "TraitDecl"
      TraitDeclGroup <$> parseJSON v
    Object o | H.lookup "TraitImpl" o /= Nothing -> do
      v <- o .: "TraitImpl"
      TraitImplGroup <$> parseJSON v
    Object o | H.lookup "Mixed" o /= Nothing -> do
      v <- o .: "Mixed"
      MixedGroup <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON Disambiguator where
  parseJSON = fmap Disambiguator . parseJSON


instance FromJSON DiscriminantLayout where
  parseJSON = withObject "DiscriminantLayout" $ \o -> do
    discriminantlayoutOffset <- o .: "offset"
    discriminantlayoutTagTy <- o .: "tag_ty"
    discriminantlayoutEncoding <- o .: "encoding"
    pure (DiscriminantLayout discriminantlayoutOffset discriminantlayoutTagTy discriminantlayoutEncoding)


instance FromJSON DynPredicate where
  parseJSON = withObject "DynPredicate" $ \o -> do
    dynpredicateBinder <- o .: "binder"
    pure (DynPredicate dynpredicateBinder)


instance FromJSON T.Field where
  parseJSON = withObject "Field" $ \o -> do
    fieldSpan <- o .: "span"
    fieldAttrInfo <- o .: "attr_info"
    fieldFieldName <- o .: "name"
    fieldFieldTy <- o .: "ty"
    pure (T.Field fieldSpan fieldAttrInfo fieldFieldName fieldFieldTy)


instance FromJSON FieldId where
  parseJSON = fmap FieldId . parseJSON


instance FromJSON FieldProjKind where
  parseJSON v = case v of
    Object o | H.lookup "Adt" o /= Nothing -> do
      withArray "ProjAdt" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.ProjAdt v0 v1)) =<< o .: "Adt"
    Object o | H.lookup "Tuple" o /= Nothing -> do
      v <- o .: "Tuple"
      E.ProjTuple <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON File where
  parseJSON = withObject "File" $ \o -> do
    fileName <- o .: "name"
    fileCrateName <- o .: "crate_name"
    fileContents <- o .: "contents"
    pure (File fileName fileCrateName fileContents)


instance FromJSON FileId where
  parseJSON = fmap FileId . parseJSON


instance FromJSON FileName where
  parseJSON v = case v of
    Object o | H.lookup "Virtual" o /= Nothing -> do
      v <- o .: "Virtual"
      M.Virtual <$> parseJSON v
    Object o | H.lookup "Local" o /= Nothing -> do
      v <- o .: "Local"
      M.Local <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON FloatType where
  parseJSON v = case v of
    String "F16" -> pure T.F16
    String "F32" -> pure T.F32
    String "F64" -> pure T.F64
    String "F128" -> pure T.F128
    _ -> fail "Unknown variant"


instance FromJSON FloatValue where
  parseJSON = withObject "FloatValue" $ \o -> do
    floatvalueFloatValue <- o .: "value"
    floatvalueFloatTy <- o .: "ty"
    pure (FloatValue floatvalueFloatValue floatvalueFloatTy)


instance FromJSON FnOperand where
  parseJSON v = case v of
    Object o | H.lookup "Regular" o /= Nothing -> do
      v <- o .: "Regular"
      FnOpRegular <$> parseJSON v
    Object o | H.lookup "Move" o /= Nothing -> do
      v <- o .: "Move"
      FnOpMove <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON FnPtr where
  parseJSON = withObject "FnPtr" $ \o -> do
    fnptrKind <- o .: "kind"
    fnptrGenerics <- o .: "generics"
    pure (FnPtr fnptrKind fnptrGenerics)


instance FromJSON FnPtrKind where
  parseJSON v = case v of
    Object o | H.lookup "Fun" o /= Nothing -> do
      v <- o .: "Fun"
      T.FunId <$> parseJSON v
    Object o | H.lookup "Trait" o /= Nothing -> do
      withArray "TraitMethod" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (T.TraitMethod v0 v1 v2)) =<< o .: "Trait"
    _ -> fail "Unknown variant"


instance FromJSON FunDeclId where
  parseJSON = fmap FunDeclId . parseJSON


instance FromJSON FunDeclRef where
  parseJSON = withObject "FunDeclRef" $ \o -> do
    fundeclrefId <- o .: "id"
    fundeclrefGenerics <- o .: "generics"
    pure (FunDeclRef fundeclrefId fundeclrefGenerics)


instance FromJSON FunId where
  parseJSON v = case v of
    Object o | H.lookup "Regular" o /= Nothing -> do
      v <- o .: "Regular"
      T.FRegular <$> parseJSON v
    Object o | H.lookup "Builtin" o /= Nothing -> do
      v <- o .: "Builtin"
      T.FBuiltin <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON FunSig where
  parseJSON = withObject "FunSig" $ \o -> do
    funsigIsUnsafe <- o .: "is_unsafe"
    funsigGenerics <- o .: "generics"
    funsigInputs <- o .: "inputs"
    funsigOutput <- o .: "output"
    pure (FunSig funsigIsUnsafe funsigGenerics funsigInputs funsigOutput)


instance (FromJSON a0) => FromJSON (GDeclarationGroup a0) where
  parseJSON v = case v of
    Object o | H.lookup "NonRec" o /= Nothing -> do
      v <- o .: "NonRec"
      NonRecGroup <$> parseJSON v
    Object o | H.lookup "Rec" o /= Nothing -> do
      v <- o .: "Rec"
      RecGroup <$> parseJSON v
    _ -> fail "Unknown variant"


instance (FromJSON a0) => FromJSON (GexprBody a0) where
  parseJSON = withObject "GexprBody" $ \o -> do
    gexprbodySpan <- o .: "span"
    gexprbodyLocals <- o .: "locals"
    gexprbodyBody <- o .: "body"
    pure (GexprBody gexprbodySpan gexprbodyLocals gexprbodyBody)


instance FromJSON GenericArgs where
  parseJSON = withObject "GenericArgs" $ \o -> do
    genericargsRegions <- o .: "regions"
    genericargsTypes <- o .: "types"
    genericargsConstGenerics <- o .: "const_generics"
    genericargsTraitRefs <- o .: "trait_refs"
    pure (GenericArgs genericargsRegions genericargsTypes genericargsConstGenerics genericargsTraitRefs)


instance FromJSON GenericParams where
  parseJSON = withObject "GenericParams" $ \o -> do
    genericparamsRegions <- o .: "regions"
    genericparamsTypes <- o .: "types"
    genericparamsConstGenerics <- o .: "const_generics"
    genericparamsTraitClauses <- o .: "trait_clauses"
    genericparamsRegionsOutlive <- o .: "regions_outlive"
    genericparamsTypesOutlive <- o .: "types_outlive"
    genericparamsTraitTypeConstraints <- o .: "trait_type_constraints"
    pure (GenericParams genericparamsRegions genericparamsTypes genericparamsConstGenerics genericparamsTraitClauses genericparamsRegionsOutlive genericparamsTypesOutlive genericparamsTraitTypeConstraints)


instance FromJSON GlobalDecl where
  parseJSON = withObject "GlobalDecl" $ \o -> do
    globaldeclDefId <- o .: "def_id"
    globaldeclItemMeta <- o .: "item_meta"
    globaldeclGenerics <- o .: "generics"
    globaldeclTy <- o .: "ty"
    globaldeclSrc <- o .: "src"
    globaldeclGlobalKind <- o .: "global_kind"
    globaldeclInit <- o .: "init"
    pure (GlobalDecl globaldeclDefId globaldeclItemMeta globaldeclGenerics globaldeclTy globaldeclSrc globaldeclGlobalKind globaldeclInit)


instance FromJSON GlobalDeclId where
  parseJSON = fmap GlobalDeclId . parseJSON


instance FromJSON GlobalDeclRef where
  parseJSON = withObject "GlobalDeclRef" $ \o -> do
    globaldeclrefId <- o .: "id"
    globaldeclrefGenerics <- o .: "generics"
    pure (GlobalDeclRef globaldeclrefId globaldeclrefGenerics)


instance FromJSON GlobalKind where
  parseJSON v = case v of
    String "Static" -> pure Static
    String "NamedConst" -> pure NamedConst
    String "AnonConst" -> pure AnonConst
    _ -> fail "Unknown variant"


instance FromJSON ImplElem where
  parseJSON v = case v of
    Object o | H.lookup "Ty" o /= Nothing -> do
      v <- o .: "Ty"
      T.ImplElemTy <$> parseJSON v
    Object o | H.lookup "Trait" o /= Nothing -> do
      v <- o .: "Trait"
      T.ImplElemTrait <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON InlineAttr where
  parseJSON v = case v of
    String "Hint" -> pure M.Hint
    String "Never" -> pure M.Never
    String "Always" -> pure M.Always
    _ -> fail "Unknown variant"


instance FromJSON IntTy where
  parseJSON v = case v of
    String "Isize" -> pure T.Isize
    String "I8" -> pure T.I8
    String "I16" -> pure T.I16
    String "I32" -> pure T.I32
    String "I64" -> pure T.I64
    String "I128" -> pure T.I128
    _ -> fail "Unknown variant"


instance FromJSON IntegerType where
  parseJSON v = case v of
    Object o | H.lookup "Signed" o /= Nothing -> do
      v <- o .: "Signed"
      T.Signed <$> parseJSON v
    Object o | H.lookup "Unsigned" o /= Nothing -> do
      v <- o .: "Unsigned"
      T.Unsigned <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON ItemId where
  parseJSON v = case v of
    Object o | H.lookup "Type" o /= Nothing -> do
      v <- o .: "Type"
      T.IdType <$> parseJSON v
    Object o | H.lookup "Fun" o /= Nothing -> do
      v <- o .: "Fun"
      T.IdFun <$> parseJSON v
    Object o | H.lookup "Global" o /= Nothing -> do
      v <- o .: "Global"
      T.IdGlobal <$> parseJSON v
    Object o | H.lookup "TraitDecl" o /= Nothing -> do
      v <- o .: "TraitDecl"
      T.IdTraitDecl <$> parseJSON v
    Object o | H.lookup "TraitImpl" o /= Nothing -> do
      v <- o .: "TraitImpl"
      T.IdTraitImpl <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON ItemMeta where
  parseJSON = withObject "ItemMeta" $ \o -> do
    itemmetaName <- o .: "name"
    itemmetaSpan <- o .: "span"
    itemmetaSourceText <- o .: "source_text"
    itemmetaAttrInfo <- o .: "attr_info"
    itemmetaIsLocal <- o .: "is_local"
    itemmetaLangItem <- o .: "lang_item"
    pure (ItemMeta itemmetaName itemmetaSpan itemmetaSourceText itemmetaAttrInfo itemmetaIsLocal itemmetaLangItem)


instance FromJSON ItemSource where
  parseJSON v = case v of
    String "TopLevel" -> pure T.TopLevelItem
    Object o | H.lookup "Closure" o /= Nothing -> do
      obj <- o .: "Closure"
      info <- obj .: "info"
      pure (T.ClosureItem info)
    Object o | H.lookup "TraitDecl" o /= Nothing -> do
      obj <- o .: "TraitDecl"
      traitRef <- obj .: "trait_ref"
      itemName <- obj .: "item_name"
      hasDefault <- obj .: "has_default"
      pure (T.TraitDeclItem traitRef itemName hasDefault)
    Object o | H.lookup "TraitImpl" o /= Nothing -> do
      obj <- o .: "TraitImpl"
      implRef <- obj .: "impl_ref"
      traitRef <- obj .: "trait_ref"
      itemName <- obj .: "item_name"
      reusesDefault <- obj .: "reuses_default"
      pure (T.TraitImplItem implRef traitRef itemName reusesDefault)
    Object o | H.lookup "VTableTy" o /= Nothing -> do
      obj <- o .: "VTableTy"
      dynPredicate <- obj .: "dyn_predicate"
      pure (T.VTableTyItem dynPredicate)
    Object o | H.lookup "VTableInstance" o /= Nothing -> do
      obj <- o .: "VTableInstance"
      implRef <- obj .: "impl_ref"
      pure (T.VTableInstanceItem implRef)
    String "VTableMethodShim" -> pure T.VTableMethodShimItem
    _ -> fail "Unknown variant"


instance FromJSON Layout where
  parseJSON = withObject "Layout" $ \o -> do
    layoutSize <- o .: "size"
    layoutAlign <- o .: "align"
    layoutDiscriminantLayout <- o .: "discriminant_layout"
    layoutUninhabited <- o .: "uninhabited"
    layoutVariantLayouts <- o .: "variant_layouts"
    pure (Layout layoutSize layoutAlign layoutDiscriminantLayout layoutUninhabited layoutVariantLayouts)


instance FromJSON Literal where
  parseJSON v = case v of
    Object o | H.lookup "Scalar" o /= Nothing -> do
      v <- o .: "Scalar"
      T.VScalar <$> parseJSON v
    Object o | H.lookup "Float" o /= Nothing -> do
      v <- o .: "Float"
      T.VFloat <$> parseJSON v
    Object o | H.lookup "Bool" o /= Nothing -> do
      v <- o .: "Bool"
      T.VBool <$> parseJSON v
    Object o | H.lookup "Char" o /= Nothing -> do
      v <- o .: "Char"
      T.VChar <$> parseJSON v
    Object o | H.lookup "ByteStr" o /= Nothing -> do
      v <- o .: "ByteStr"
      T.VByteStr <$> parseJSON v
    Object o | H.lookup "Str" o /= Nothing -> do
      v <- o .: "Str"
      T.VStr <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON LiteralType where
  parseJSON v = case v of
    Object o | H.lookup "Int" o /= Nothing -> do
      v <- o .: "Int"
      T.TInt <$> parseJSON v
    Object o | H.lookup "UInt" o /= Nothing -> do
      v <- o .: "UInt"
      T.TuInt <$> parseJSON v
    Object o | H.lookup "Float" o /= Nothing -> do
      v <- o .: "Float"
      T.TFloat <$> parseJSON v
    String "Bool" -> pure T.TBool
    String "Char" -> pure T.TChar
    _ -> fail "Unknown variant"


instance FromJSON Loc where
  parseJSON = withObject "Loc" $ \o -> do
    locLine <- o .: "line"
    locCol <- o .: "col"
    pure (Loc locLine locCol)


instance FromJSON G.Local where
  parseJSON = withObject "Local" $ \o -> do
    localIndex <- o .: "index"
    localName <- o .: "name"
    localLocalTy <- o .: "ty"
    pure (G.Local localIndex localName localLocalTy)


instance FromJSON LocalId where
  parseJSON = fmap LocalId . parseJSON


instance FromJSON Locals where
  parseJSON = withObject "Locals" $ \o -> do
    localsArgCount <- o .: "arg_count"
    localsLocals <- o .: "locals"
    pure (Locals localsArgCount localsLocals)


instance FromJSON MirLevel where
  parseJSON v = case v of
    String "Built" -> pure Built
    String "Promoted" -> pure Promoted
    String "Elaborated" -> pure Elaborated
    String "Optimized" -> pure Optimized
    _ -> fail "Unknown variant"


instance FromJSON MonomorphizeMut where
  parseJSON v = case v of
    String "All" -> pure All
    String "ExceptTypes" -> pure ExceptTypes
    _ -> fail "Unknown variant"


instance FromJSON Name where
  parseJSON = fmap Name . parseJSON


instance FromJSON Nullop where
  parseJSON v = case v of
    String "SizeOf" -> pure E.SizeOf
    String "AlignOf" -> pure E.AlignOf
    Object o | H.lookup "OffsetOf" o /= Nothing -> do
      v <- o .: "OffsetOf"
      E.OffsetOf <$> parseJSON v
    String "UbChecks" -> pure E.UbChecks
    _ -> fail "Unknown variant"


instance FromJSON Operand where
  parseJSON v = case v of
    Object o | H.lookup "Copy" o /= Nothing -> do
      v <- o .: "Copy"
      E.Copy <$> parseJSON v
    Object o | H.lookup "Move" o /= Nothing -> do
      v <- o .: "Move"
      E.Move <$> parseJSON v
    Object o | H.lookup "Const" o /= Nothing -> do
      v <- o .: "Const"
      E.Constant <$> parseJSON v
    _ -> fail "Unknown variant"


instance (FromJSON a0, FromJSON a1) => FromJSON (OutlivesPred a0 a1) where
  parseJSON = withArray "OutlivesPred" $ \v -> do
    v0 <- parseJSON (v V.! 0)
    v1 <- parseJSON (v V.! 1)
    pure (OutlivesPred v0 v1)


instance FromJSON OverflowMode where
  parseJSON v = case v of
    String "Panic" -> pure E.OPanic
    String "UB" -> pure E.Oub
    String "Wrap" -> pure E.OWrap
    _ -> fail "Unknown variant"


instance FromJSON PathElem where
  parseJSON v = case v of
    Object o | H.lookup "Ident" o /= Nothing -> do
      withArray "PeIdent" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (T.PeIdent v0 v1)) =<< o .: "Ident"
    Object o | H.lookup "Impl" o /= Nothing -> do
      v <- o .: "Impl"
      T.PeImpl <$> parseJSON v
    Object o | H.lookup "Instantiated" o /= Nothing -> do
      v <- o .: "Instantiated"
      T.PeInstantiated <$> parseJSON v
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
      E.PlaceLocal <$> parseJSON v
    Object o | H.lookup "Projection" o /= Nothing -> do
      withArray "PlaceProjection" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.PlaceProjection v0 v1)) =<< o .: "Projection"
    Object o | H.lookup "Global" o /= Nothing -> do
      v <- o .: "Global"
      E.PlaceGlobal <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON Preset where
  parseJSON v = case v of
    String "OldDefaults" -> pure OldDefaults
    String "Aeneas" -> pure Aeneas
    String "Eurydice" -> pure Eurydice
    String "Soteria" -> pure Soteria
    String "Tests" -> pure Tests
    _ -> fail "Unknown variant"


instance FromJSON ProjectionElem where
  parseJSON v = case v of
    String "Deref" -> pure E.Deref
    Object o | H.lookup "Field" o /= Nothing -> do
      withArray "Field" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.Field v0 v1)) =<< o .: "Field"
    String "PtrMetadata" -> pure E.PtrMetadata
    Object o | H.lookup "Index" o /= Nothing -> do
      obj <- o .: "Index"
      offset <- obj .: "offset"
      fromEnd <- obj .: "from_end"
      pure (E.ProjIndex offset fromEnd)
    Object o | H.lookup "Subslice" o /= Nothing -> do
      obj <- o .: "Subslice"
      from <- obj .: "from"
      to <- obj .: "to"
      fromEnd <- obj .: "from_end"
      pure (E.Subslice from to fromEnd)
    _ -> fail "Unknown variant"


instance FromJSON T.PtrMetadata where
  parseJSON v = case v of
    String "None" -> pure T.NoMetadata
    String "Length" -> pure T.Length
    Object o | H.lookup "VTable" o /= Nothing -> do
      v <- o .: "VTable"
      T.VTable <$> parseJSON v
    Object o | H.lookup "InheritFrom" o /= Nothing -> do
      v <- o .: "InheritFrom"
      T.InheritFrom <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON RawAttribute where
  parseJSON = withObject "RawAttribute" $ \o -> do
    rawattributePath <- o .: "path"
    rawattributeArgs <- o .: "args"
    pure (RawAttribute rawattributePath rawattributeArgs)


instance FromJSON RefKind where
  parseJSON v = case v of
    String "Mut" -> pure T.RMut
    String "Shared" -> pure T.RShared
    _ -> fail "Unknown variant"


instance FromJSON Region where
  parseJSON v = case v of
    Object o | H.lookup "Var" o /= Nothing -> do
      v <- o .: "Var"
      T.RVar <$> parseJSON v
    String "Static" -> pure T.RStatic
    String "Erased" -> pure T.RErased
    _ -> fail "Unknown variant"


instance (FromJSON a0) => FromJSON (RegionBinder a0) where
  parseJSON = withObject "RegionBinder" $ \o -> do
    regionbinderBinderRegions <- o .: "regions"
    regionbinderBinderValue <- o .: "skip_binder"
    pure (RegionBinder regionbinderBinderRegions regionbinderBinderValue)


instance FromJSON RegionId where
  parseJSON = fmap RegionId . parseJSON


instance FromJSON RegionParam where
  parseJSON = withObject "RegionParam" $ \o -> do
    regionparamIndex <- o .: "index"
    regionparamName <- o .: "name"
    pure (RegionParam regionparamIndex regionparamName)


instance FromJSON ReprAlgorithm where
  parseJSON v = case v of
    String "Rust" -> pure T.Rust
    String "C" -> pure T.C
    _ -> fail "Unknown variant"


instance FromJSON ReprOptions where
  parseJSON = withObject "ReprOptions" $ \o -> do
    reproptionsReprAlgo <- o .: "repr_algo"
    reproptionsAlignModif <- o .: "align_modif"
    reproptionsTransparent <- o .: "transparent"
    reproptionsExplicitDiscrType <- o .: "explicit_discr_type"
    pure (ReprOptions reproptionsReprAlgo reproptionsAlignModif reproptionsTransparent reproptionsExplicitDiscrType)


instance FromJSON Rvalue where
  parseJSON v = case v of
    Object o | H.lookup "Use" o /= Nothing -> do
      v <- o .: "Use"
      E.Use <$> parseJSON v
    Object o | H.lookup "Ref" o /= Nothing -> do
      obj <- o .: "Ref"
      place <- obj .: "place"
      kind <- obj .: "kind"
      ptrMetadata <- obj .: "ptr_metadata"
      pure (E.RvRef place kind ptrMetadata)
    Object o | H.lookup "RawPtr" o /= Nothing -> do
      obj <- o .: "RawPtr"
      place <- obj .: "place"
      kind <- obj .: "kind"
      ptrMetadata <- obj .: "ptr_metadata"
      pure (E.RawPtr place kind ptrMetadata)
    Object o | H.lookup "BinaryOp" o /= Nothing -> do
      withArray "BinaryOp" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (E.BinaryOp v0 v1 v2)) =<< o .: "BinaryOp"
    Object o | H.lookup "UnaryOp" o /= Nothing -> do
      withArray "UnaryOp" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.UnaryOp v0 v1)) =<< o .: "UnaryOp"
    Object o | H.lookup "NullaryOp" o /= Nothing -> do
      withArray "NullaryOp" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.NullaryOp v0 v1)) =<< o .: "NullaryOp"
    Object o | H.lookup "Discriminant" o /= Nothing -> do
      v <- o .: "Discriminant"
      E.Discriminant <$> parseJSON v
    Object o | H.lookup "Aggregate" o /= Nothing -> do
      withArray "Aggregate" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.Aggregate v0 v1)) =<< o .: "Aggregate"
    Object o | H.lookup "Len" o /= Nothing -> do
      withArray "Len" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (E.Len v0 v1 v2)) =<< o .: "Len"
    Object o | H.lookup "Repeat" o /= Nothing -> do
      withArray "Repeat" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (E.Repeat v0 v1 v2)) =<< o .: "Repeat"
    Object o | H.lookup "ShallowInitBox" o /= Nothing -> do
      withArray "ShallowInitBox" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (E.ShallowInitBox v0 v1)) =<< o .: "ShallowInitBox"
    _ -> fail "Unknown variant"


instance FromJSON ScalarValue where
  parseJSON v = case v of
    Object o | H.lookup "Unsigned" o /= Nothing -> do
      withArray "UnsignedScalar" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseIntegerValue (v V.! 1)
        pure (UnsignedScalar v0 v1)) =<< o .: "Unsigned"
    Object o | H.lookup "Signed" o /= Nothing -> do
      withArray "SignedScalar" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseIntegerValue (v V.! 1)
        pure (SignedScalar v0 v1)) =<< o .: "Signed"
    _ -> fail "Unknown variant"


instance FromJSON Span where
  parseJSON = withObject "Span" $ \o -> do
    spanData <- o .: "data"
    spanGeneratedFromSpan <- o .: "generated_from_span"
    pure (Span spanData spanGeneratedFromSpan)


instance FromJSON SpanData where
  parseJSON = withObject "SpanData" $ \o -> do
    spandataFile <- o .: "file_id"
    spandataBegLoc <- o .: "beg"
    spandataEndLoc <- o .: "end"
    pure (SpanData spandataFile spandataBegLoc spandataEndLoc)


instance FromJSON TagEncoding where
  parseJSON v = case v of
    String "Direct" -> pure T.Direct
    Object o | H.lookup "Niche" o /= Nothing -> do
      obj <- o .: "Niche"
      untaggedVariant <- obj .: "untagged_variant"
      pure (T.Niche untaggedVariant)
    _ -> fail "Unknown variant"


instance FromJSON TargetInfo where
  parseJSON = withObject "TargetInfo" $ \o -> do
    targetinfoTargetPointerSize <- o .: "target_pointer_size"
    targetinfoIsLittleEndian <- o .: "is_little_endian"
    pure (TargetInfo targetinfoTargetPointerSize targetinfoIsLittleEndian)


instance FromJSON TraitAssocConst where
  parseJSON = withObject "TraitAssocConst" $ \o -> do
    traitassocconstName <- o .: "name"
    traitassocconstTy <- o .: "ty"
    traitassocconstDefault <- o .: "default"
    pure (TraitAssocConst traitassocconstName traitassocconstTy traitassocconstDefault)


instance FromJSON TraitAssocTy where
  parseJSON = withObject "TraitAssocTy" $ \o -> do
    traitassoctyName <- o .: "name"
    traitassoctyDefault <- o .: "default"
    traitassoctyImpliedClauses <- o .: "implied_clauses"
    pure (TraitAssocTy traitassoctyName traitassoctyDefault traitassoctyImpliedClauses)


instance FromJSON TraitAssocTyImpl where
  parseJSON = withObject "TraitAssocTyImpl" $ \o -> do
    traitassoctyimplValue <- o .: "value"
    pure (TraitAssocTyImpl traitassoctyimplValue)


instance FromJSON TraitClauseId where
  parseJSON = fmap TraitClauseId . parseJSON


instance FromJSON TraitDecl where
  parseJSON = withObject "TraitDecl" $ \o -> do
    traitdeclDefId <- o .: "def_id"
    traitdeclItemMeta <- o .: "item_meta"
    traitdeclGenerics <- o .: "generics"
    traitdeclImpliedClauses <- o .: "implied_clauses"
    traitdeclConsts <- o .: "consts"
    traitdeclTypes <- o .: "types"
    traitdeclMethods <- o .: "methods"
    traitdeclVtable <- o .: "vtable"
    pure (TraitDecl traitdeclDefId traitdeclItemMeta traitdeclGenerics traitdeclImpliedClauses traitdeclConsts traitdeclTypes traitdeclMethods traitdeclVtable)


instance FromJSON TraitDeclId where
  parseJSON = fmap TraitDeclId . parseJSON


instance FromJSON TraitDeclRef where
  parseJSON = withObject "TraitDeclRef" $ \o -> do
    traitdeclrefId <- o .: "id"
    traitdeclrefGenerics <- o .: "generics"
    pure (TraitDeclRef traitdeclrefId traitdeclrefGenerics)


instance FromJSON G.TraitImpl where
  parseJSON = withObject "TraitImpl" $ \o -> do
    traitimplDefId <- o .: "def_id"
    traitimplItemMeta <- o .: "item_meta"
    traitimplImplTrait <- o .: "impl_trait"
    traitimplGenerics <- o .: "generics"
    traitimplImpliedTraitRefs <- o .: "implied_trait_refs"
    traitimplConsts <- o .: "consts"
    traitimplTypes <- o .: "types"
    traitimplMethods <- o .: "methods"
    traitimplVtable <- o .: "vtable"
    pure (G.TraitImpl traitimplDefId traitimplItemMeta traitimplImplTrait traitimplGenerics traitimplImpliedTraitRefs traitimplConsts traitimplTypes traitimplMethods traitimplVtable)


instance FromJSON TraitImplId where
  parseJSON = fmap TraitImplId . parseJSON


instance FromJSON TraitImplRef where
  parseJSON = withObject "TraitImplRef" $ \o -> do
    traitimplrefId <- o .: "id"
    traitimplrefGenerics <- o .: "generics"
    pure (TraitImplRef traitimplrefId traitimplrefGenerics)


instance FromJSON TraitItemName where
  parseJSON = fmap TraitItemName . parseJSON


instance FromJSON G.TraitMethod where
  parseJSON = withObject "TraitMethod" $ \o -> do
    traitmethodName <- o .: "name"
    traitmethodItem <- o .: "item"
    pure (G.TraitMethod traitmethodName traitmethodItem)


instance FromJSON TraitParam where
  parseJSON = withObject "TraitParam" $ \o -> do
    traitparamClauseId <- o .: "clause_id"
    traitparamSpan <- o .: "span"
    traitparamTrait <- o .: "trait_"
    pure (TraitParam traitparamClauseId traitparamSpan traitparamTrait)


instance FromJSON TraitRef where
  parseJSON = withObject "TraitRef" $ \o -> do
    traitrefKind <- o .: "kind"
    traitrefTraitDeclRef <- o .: "trait_decl_ref"
    pure (TraitRef traitrefKind traitrefTraitDeclRef)


instance FromJSON TraitRefKind where
  parseJSON v = case v of
    Object o | H.lookup "TraitImpl" o /= Nothing -> do
      v <- o .: "TraitImpl"
      T.TraitImpl <$> parseJSON v
    Object o | H.lookup "Clause" o /= Nothing -> do
      v <- o .: "Clause"
      T.Clause <$> parseJSON v
    Object o | H.lookup "ParentClause" o /= Nothing -> do
      withArray "ParentClause" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (T.ParentClause v0 v1)) =<< o .: "ParentClause"
    Object o | H.lookup "ItemClause" o /= Nothing -> do
      withArray "ItemClause" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (T.ItemClause v0 v1 v2)) =<< o .: "ItemClause"
    String "SelfId" -> pure T.Self
    Object o | H.lookup "BuiltinOrAuto" o /= Nothing -> do
      obj <- o .: "BuiltinOrAuto"
      builtinData <- obj .: "builtin_data"
      parentTraitRefs <- obj .: "parent_trait_refs"
      types <- obj .: "types"
      pure (T.BuiltinOrAuto builtinData parentTraitRefs types)
    String "Dyn" -> pure T.Dyn
    Object o | H.lookup "Unknown" o /= Nothing -> do
      v <- o .: "Unknown"
      T.UnknownTrait <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON TraitTypeConstraint where
  parseJSON = withObject "TraitTypeConstraint" $ \o -> do
    traittypeconstraintTraitRef <- o .: "trait_ref"
    traittypeconstraintTypeName <- o .: "type_name"
    traittypeconstraintTy <- o .: "ty"
    pure (TraitTypeConstraint traittypeconstraintTraitRef traittypeconstraintTypeName traittypeconstraintTy)


instance FromJSON TraitTypeConstraintId where
  parseJSON = fmap TraitTypeConstraintId . parseJSON


instance FromJSON Ty where
  parseJSON v = case v of
    Object o | H.lookup "Adt" o /= Nothing -> do
      v <- o .: "Adt"
      T.TAdt <$> parseJSON v
    Object o | H.lookup "TypeVar" o /= Nothing -> do
      v <- o .: "TypeVar"
      T.TVar <$> parseJSON v
    Object o | H.lookup "Literal" o /= Nothing -> do
      v <- o .: "Literal"
      T.TLiteral <$> parseJSON v
    String "Never" -> pure T.TNever
    Object o | H.lookup "Ref" o /= Nothing -> do
      withArray "TRef" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        v2 <- parseJSON (v V.! 2)
        pure (T.TRef v0 v1 v2)) =<< o .: "Ref"
    Object o | H.lookup "RawPtr" o /= Nothing -> do
      withArray "TRawPtr" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (T.TRawPtr v0 v1)) =<< o .: "RawPtr"
    Object o | H.lookup "TraitType" o /= Nothing -> do
      withArray "TTraitType" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseJSON (v V.! 1)
        pure (T.TTraitType v0 v1)) =<< o .: "TraitType"
    Object o | H.lookup "DynTrait" o /= Nothing -> do
      v <- o .: "DynTrait"
      T.TDynTrait <$> parseJSON v
    Object o | H.lookup "FnPtr" o /= Nothing -> do
      v <- o .: "FnPtr"
      T.TFnPtr <$> parseJSON v
    Object o | H.lookup "FnDef" o /= Nothing -> do
      v <- o .: "FnDef"
      T.TFnDef <$> parseJSON v
    Object o | H.lookup "PtrMetadata" o /= Nothing -> do
      v <- o .: "PtrMetadata"
      T.TPtrMetadata <$> parseJSON v
    Object o | H.lookup "Error" o /= Nothing -> do
      v <- o .: "Error"
      T.TError <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON TypeDecl where
  parseJSON = withObject "TypeDecl" $ \o -> do
    typedeclDefId <- o .: "def_id"
    typedeclItemMeta <- o .: "item_meta"
    typedeclGenerics <- o .: "generics"
    typedeclSrc <- o .: "src"
    typedeclKind <- o .: "kind"
    typedeclLayout <- o .: "layout"
    typedeclPtrMetadata <- o .: "ptr_metadata"
    typedeclRepr <- o .: "repr"
    pure (TypeDecl typedeclDefId typedeclItemMeta typedeclGenerics typedeclSrc typedeclKind typedeclLayout typedeclPtrMetadata typedeclRepr)


instance FromJSON TypeDeclId where
  parseJSON = fmap TypeDeclId . parseJSON


instance FromJSON TypeDeclKind where
  parseJSON v = case v of
    Object o | H.lookup "Struct" o /= Nothing -> do
      v <- o .: "Struct"
      T.Struct <$> parseJSON v
    Object o | H.lookup "Enum" o /= Nothing -> do
      v <- o .: "Enum"
      T.Enum <$> parseJSON v
    Object o | H.lookup "Union" o /= Nothing -> do
      v <- o .: "Union"
      T.Union <$> parseJSON v
    String "Opaque" -> pure T.Opaque
    Object o | H.lookup "Alias" o /= Nothing -> do
      v <- o .: "Alias"
      T.Alias <$> parseJSON v
    Object o | H.lookup "Error" o /= Nothing -> do
      v <- o .: "Error"
      T.TDeclError <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON TypeDeclRef where
  parseJSON = withObject "TypeDeclRef" $ \o -> do
    typedeclrefId <- o .: "id"
    typedeclrefGenerics <- o .: "generics"
    pure (TypeDeclRef typedeclrefId typedeclrefGenerics)


instance FromJSON TypeId where
  parseJSON v = case v of
    Object o | H.lookup "Adt" o /= Nothing -> do
      v <- o .: "Adt"
      T.TAdtId <$> parseJSON v
    String "Tuple" -> pure T.TTuple
    Object o | H.lookup "Builtin" o /= Nothing -> do
      v <- o .: "Builtin"
      T.TBuiltin <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON TypeParam where
  parseJSON = withObject "TypeParam" $ \o -> do
    typeparamIndex <- o .: "index"
    typeparamName <- o .: "name"
    pure (TypeParam typeparamIndex typeparamName)


instance FromJSON TypeVarId where
  parseJSON = fmap TypeVarId . parseJSON


instance FromJSON UIntTy where
  parseJSON v = case v of
    String "Usize" -> pure T.Usize
    String "U8" -> pure T.U8
    String "U16" -> pure T.U16
    String "U32" -> pure T.U32
    String "U64" -> pure T.U64
    String "U128" -> pure T.U128
    _ -> fail "Unknown variant"


instance FromJSON Unop where
  parseJSON v = case v of
    String "Not" -> pure E.Not
    Object o | H.lookup "Neg" o /= Nothing -> do
      v <- o .: "Neg"
      E.Neg <$> parseJSON v
    Object o | H.lookup "Cast" o /= Nothing -> do
      v <- o .: "Cast"
      E.Cast <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON UnsizingMetadata where
  parseJSON v = case v of
    Object o | H.lookup "Length" o /= Nothing -> do
      v <- o .: "Length"
      E.MetaLength <$> parseJSON v
    Object o | H.lookup "VTablePtr" o /= Nothing -> do
      v <- o .: "VTablePtr"
      E.MetaVTablePtr <$> parseJSON v
    String "Unknown" -> pure E.MetaUnknown
    _ -> fail "Unknown variant"


instance FromJSON Variant where
  parseJSON = withObject "Variant" $ \o -> do
    variantSpan <- o .: "span"
    variantAttrInfo <- o .: "attr_info"
    variantVariantName <- o .: "name"
    variantFields <- o .: "fields"
    variantDiscriminant <- o .: "discriminant"
    pure (Variant variantSpan variantAttrInfo variantVariantName variantFields variantDiscriminant)


instance FromJSON VariantId where
  parseJSON = fmap VariantId . parseJSON


instance FromJSON VariantLayout where
  parseJSON = withObject "VariantLayout" $ \o -> do
    variantlayoutFieldOffsets <- o .: "field_offsets"
    variantlayoutUninhabited <- o .: "uninhabited"
    variantlayoutTag <- o .: "tag"
    pure (VariantLayout variantlayoutFieldOffsets variantlayoutUninhabited variantlayoutTag)

