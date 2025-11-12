{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_GAstOfJson.hs`
by hand. Edit `templates/GAstOfJson.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_GAstOfJson where

import Data.Aeson
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions
import Generated_GAst

instance FromJSON AbortKind where
  parseJSON v = case v of
  Object o | H.lookup "Panic" o /= Nothing -> do
  v <- o .: "Panic"
  Panic <$> parseJSON v

    String "UndefinedBehavior" -> pure UndefinedBehavior
    String "UnwindTerminate" -> pure UnwindTerminate
  _ -> fail "Unknown variant"


instance FromJSON AggregateKind where
  parseJSON v = case v of
  Object o | H.lookup "Adt" o /= Nothing -> do
  arr <- o .: "Adt"
  withArray "AggregatedAdt" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (AggregatedAdt v0 v1 v2)) arr

    Object o | H.lookup "Array" o /= Nothing -> do
  arr <- o .: "Array"
  withArray "AggregatedArray" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (AggregatedArray v0 v1)) arr

    Object o | H.lookup "RawPtr" o /= Nothing -> do
  arr <- o .: "RawPtr"
  withArray "AggregatedRawPtr" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (AggregatedRawPtr v0 v1)) arr

  _ -> fail "Unknown variant"


instance FromJSON AlignmentModifier where
  parseJSON v = case v of
  Object o | H.lookup "Align" o /= Nothing -> do
  v <- o .: "Align"
  Align <$> parseJSON v

    Object o | H.lookup "Pack" o /= Nothing -> do
  v <- o .: "Pack"
  Pack <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON Assertion where
  parseJSON = withObject "Assertion" $ \o -> do
  cond <- o .: "cond"
    expected <- o .: "expected"
    onFailure <- o .: "on_failure"
  pure Assertion { cond, expected, onFailure }


instance FromJSON AttrInfo where
  parseJSON = withObject "AttrInfo" $ \o -> do
  attributes <- o .: "attributes"
    inline <- o .: "inline"
    rename <- o .: "rename"
    public <- o .: "public"
  pure AttrInfo { attributes, inline, rename, public }


instance FromJSON Attribute where
  parseJSON v = case v of
  String "Opaque" -> pure AttrOpaque
    Object o | H.lookup "Rename" o /= Nothing -> do
  v <- o .: "Rename"
  AttrRename <$> parseJSON v

    Object o | H.lookup "VariantsPrefix" o /= Nothing -> do
  v <- o .: "VariantsPrefix"
  AttrVariantsPrefix <$> parseJSON v

    Object o | H.lookup "VariantsSuffix" o /= Nothing -> do
  v <- o .: "VariantsSuffix"
  AttrVariantsSuffix <$> parseJSON v

    Object o | H.lookup "DocComment" o /= Nothing -> do
  v <- o .: "DocComment"
  AttrDocComment <$> parseJSON v

    Object o | H.lookup "Unknown" o /= Nothing -> do
  v <- o .: "Unknown"
  AttrUnknown <$> parseJSON v

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


instance (FromJSON a0) => FromJSON Binder a0 where
  parseJSON = withObject "Binder" $ \o -> do
  binderParams <- o .: "params"
    binderValue <- o .: "skip_binder"
  pure Binder { binderParams, binderValue }


instance FromJSON BinderKind where
  parseJSON v = case v of
  Object o | H.lookup "TraitType" o /= Nothing -> do
  arr <- o .: "TraitType"
  withArray "BkTraitType" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (BkTraitType v0 v1)) arr

    Object o | H.lookup "TraitMethod" o /= Nothing -> do
  arr <- o .: "TraitMethod"
  withArray "BkTraitMethod" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (BkTraitMethod v0 v1)) arr

    String "InherentImplBlock" -> pure BkInherentImplBlock
    String "Dyn" -> pure BkDyn
    String "Other" -> pure BkOther
  _ -> fail "Unknown variant"


instance FromJSON BorrowKind where
  parseJSON v = case v of
  String "Shared" -> pure BShared
    String "Mut" -> pure BMut
    String "TwoPhaseMut" -> pure BTwoPhaseMut
    String "Shallow" -> pure BShallow
    String "UniqueImmutable" -> pure BUniqueImmutable
  _ -> fail "Unknown variant"


instance FromJSON BuiltinFunId where
  parseJSON v = case v of
  String "BoxNew" -> pure BoxNew
    String "ArrayToSliceShared" -> pure ArrayToSliceShared
    String "ArrayToSliceMut" -> pure ArrayToSliceMut
    String "ArrayRepeat" -> pure ArrayRepeat
    Object o | H.lookup "Index" o /= Nothing -> do
  v <- o .: "Index"
  Index <$> parseJSON v

    Object o | H.lookup "PtrFromParts" o /= Nothing -> do
  v <- o .: "PtrFromParts"
  PtrFromParts <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON BuiltinImplData where
  parseJSON v = case v of
  String "Sized" -> pure BuiltinSized
    String "MetaSized" -> pure BuiltinMetaSized
    String "Tuple" -> pure BuiltinTuple
    String "Send" -> pure BuiltinSend
    String "Sync" -> pure BuiltinSync
    String "Pointee" -> pure BuiltinPointee
    String "DiscriminantKind" -> pure BuiltinDiscriminantKind
    String "Unpin" -> pure BuiltinUnpin
    String "Freeze" -> pure BuiltinFreeze
    String "NoopDestruct" -> pure BuiltinNoopDestruct
    String "UntrackedDestruct" -> pure BuiltinUntrackedDestruct
    String "Fn" -> pure BuiltinFn
    String "FnMut" -> pure BuiltinFnMut
    String "FnOnce" -> pure BuiltinFnOnce
    String "Copy" -> pure BuiltinCopy
    String "Clone" -> pure BuiltinClone
  _ -> fail "Unknown variant"


instance FromJSON BuiltinIndexOp where
  parseJSON = withObject "BuiltinIndexOp" $ \o -> do
  isArray <- o .: "is_array"
    mutability <- o .: "mutability"
    isRange <- o .: "is_range"
  pure BuiltinIndexOp { isArray, mutability, isRange }


instance FromJSON BuiltinTy where
  parseJSON v = case v of
  String "Box" -> pure TBox
    String "Array" -> pure TArray
    String "Slice" -> pure TSlice
    String "Str" -> pure TStr
  _ -> fail "Unknown variant"


instance FromJSON Call where
  parseJSON = withObject "Call" $ \o -> do
  func <- o .: "func"
    args <- o .: "args"
    dest <- o .: "dest"
  pure Call { func, args, dest }


instance FromJSON CastKind where
  parseJSON v = case v of
  Object o | H.lookup "Scalar" o /= Nothing -> do
  arr <- o .: "Scalar"
  withArray "CastScalar" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (CastScalar v0 v1)) arr

    Object o | H.lookup "RawPtr" o /= Nothing -> do
  arr <- o .: "RawPtr"
  withArray "CastRawPtr" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (CastRawPtr v0 v1)) arr

    Object o | H.lookup "FnPtr" o /= Nothing -> do
  arr <- o .: "FnPtr"
  withArray "CastFnPtr" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (CastFnPtr v0 v1)) arr

    Object o | H.lookup "Unsize" o /= Nothing -> do
  arr <- o .: "Unsize"
  withArray "CastUnsize" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (CastUnsize v0 v1 v2)) arr

    Object o | H.lookup "Transmute" o /= Nothing -> do
  arr <- o .: "Transmute"
  withArray "CastTransmute" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (CastTransmute v0 v1)) arr

    Object o | H.lookup "Concretize" o /= Nothing -> do
  arr <- o .: "Concretize"
  withArray "CastConcretize" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (CastConcretize v0 v1)) arr

  _ -> fail "Unknown variant"


instance FromJSON CliOptions where
  parseJSON = withObject "CliOptions" $ \o -> do
  ullbc <- o .: "ullbc"
    lib <- o .: "lib"
    bin <- o .: "bin"
    mirPromoted <- o .: "mir_promoted"
    mirOptimized <- o .: "mir_optimized"
    mir <- o .: "mir"
    inputFile <- o .: "input_file"
    readLlbc <- o .: "read_llbc"
    destDir <- o .: "dest_dir"
    destFile <- o .: "dest_file"
    usePolonius <- o .: "use_polonius"
    skipBorrowck <- o .: "skip_borrowck"
    monomorphize <- o .: "monomorphize"
    monomorphizeMut <- o .: "monomorphize_mut"
    extractOpaqueBodies <- o .: "extract_opaque_bodies"
    translateAllMethods <- o .: "translate_all_methods"
    included <- o .: "include"
    opaque <- o .: "opaque"
    exclude <- o .: "exclude"
    removeAssociatedTypes <- o .: "remove_associated_types"
    hideMarkerTraits <- o .: "hide_marker_traits"
    removeAdtClauses <- o .: "remove_adt_clauses"
    hideAllocator <- o .: "hide_allocator"
    removeUnusedSelfClauses <- o .: "remove_unused_self_clauses"
    addDropBounds <- o .: "add_drop_bounds"
    startFrom <- o .: "start_from"
    noCargo <- o .: "no_cargo"
    rustcArgs <- o .: "rustc_args"
    cargoArgs <- o .: "cargo_args"
    abortOnError <- o .: "abort_on_error"
    errorOnWarnings <- o .: "error_on_warnings"
    noSerialize <- o .: "no_serialize"
    printOriginalUllbc <- o .: "print_original_ullbc"
    printUllbc <- o .: "print_ullbc"
    printBuiltLlbc <- o .: "print_built_llbc"
    printLlbc <- o .: "print_llbc"
    noMergeGotoChains <- o .: "no_merge_goto_chains"
    noOpsToFunctionCalls <- o .: "no_ops_to_function_calls"
    rawBoxes <- o .: "raw_boxes"
    preset <- o .: "preset"
  pure CliOptions { ullbc, lib, bin, mirPromoted, mirOptimized, mir, inputFile, readLlbc, destDir, destFile, usePolonius, skipBorrowck, monomorphize, monomorphizeMut, extractOpaqueBodies, translateAllMethods, included, opaque, exclude, removeAssociatedTypes, hideMarkerTraits, removeAdtClauses, hideAllocator, removeUnusedSelfClauses, addDropBounds, startFrom, noCargo, rustcArgs, cargoArgs, abortOnError, errorOnWarnings, noSerialize, printOriginalUllbc, printUllbc, printBuiltLlbc, printLlbc, noMergeGotoChains, noOpsToFunctionCalls, rawBoxes, preset }


instance FromJSON ClosureInfo where
  parseJSON = withObject "ClosureInfo" $ \o -> do
  kind <- o .: "kind"
    fnOnceImpl <- o .: "fn_once_impl"
    fnMutImpl <- o .: "fn_mut_impl"
    fnImpl <- o .: "fn_impl"
    signature <- o .: "signature"
  pure ClosureInfo { kind, fnOnceImpl, fnMutImpl, fnImpl, signature }


instance FromJSON ClosureKind where
  parseJSON v = case v of
  String "Fn" -> pure Fn
    String "FnMut" -> pure FnMut
    String "FnOnce" -> pure FnOnce
  _ -> fail "Unknown variant"


instance FromJSON ConstGeneric where
  parseJSON v = case v of
  Object o | H.lookup "Global" o /= Nothing -> do
  v <- o .: "Global"
  CgGlobal <$> parseJSON v

    Object o | H.lookup "Var" o /= Nothing -> do
  v <- o .: "Var"
  CgVar <$> parseJSON v

    Object o | H.lookup "Value" o /= Nothing -> do
  v <- o .: "Value"
  CgValue <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON ConstGenericParam where
  parseJSON = withObject "ConstGenericParam" $ \o -> do
  index <- o .: "index"
    name <- o .: "name"
    ty <- o .: "ty"
  pure ConstGenericParam { index, name, ty }


instance FromJSON ConstGenericVarId where
  parseJSON = withObject "ConstGenericVarId" $ \o -> do
  raw <- o .: "_raw"
  pure ConstGenericVarId { raw }


instance FromJSON ConstantExpr where
  parseJSON = withObject "ConstantExpr" $ \o -> do
  kind <- o .: "kind"
    ty <- o .: "ty"
  pure ConstantExpr { kind, ty }


instance FromJSON ConstantExprKind where
  parseJSON v = case v of
  Object o | H.lookup "Literal" o /= Nothing -> do
  v <- o .: "Literal"
  CLiteral <$> parseJSON v

    Object o | H.lookup "TraitConst" o /= Nothing -> do
  arr <- o .: "TraitConst"
  withArray "CTraitConst" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (CTraitConst v0 v1)) arr

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


instance FromJSON CopyNonOverlapping where
  parseJSON = withObject "CopyNonOverlapping" $ \o -> do
  src <- o .: "src"
    dst <- o .: "dst"
    count <- o .: "count"
  pure CopyNonOverlapping { src, dst, count }


instance FromJSON DeBruijnId where
  parseJSON = withObject "DeBruijnId" $ \o -> do
  index <- o .: "index"
  pure DeBruijnId { index }


instance (FromJSON a0) => FromJSON DeBruijnVar a0 where
  parseJSON v = case v of
  Object o | H.lookup "Bound" o /= Nothing -> do
  arr <- o .: "Bound"
  withArray "Bound" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (Bound v0 v1)) arr

    Object o | H.lookup "Free" o /= Nothing -> do
  v <- o .: "Free"
  Free <$> parseJSON v

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
  parseJSON = withObject "Disambiguator" $ \o -> do
  raw <- o .: "_raw"
  pure Disambiguator { raw }


instance FromJSON DiscriminantLayout where
  parseJSON = withObject "DiscriminantLayout" $ \o -> do
  offset <- o .: "offset"
    tagTy <- o .: "tag_ty"
    encoding <- o .: "encoding"
  pure DiscriminantLayout { offset, tagTy, encoding }


instance FromJSON DynPredicate where
  parseJSON = withObject "DynPredicate" $ \o -> do
  binder <- o .: "binder"
  pure DynPredicate { binder }


instance FromJSON Field where
  parseJSON = withObject "Field" $ \o -> do
  span <- o .: "span"
    attrInfo <- o .: "attr_info"
    fieldName <- o .: "name"
    fieldTy <- o .: "ty"
  pure Field { span, attrInfo, fieldName, fieldTy }


instance FromJSON FieldId where
  parseJSON = withObject "FieldId" $ \o -> do
  raw <- o .: "_raw"
  pure FieldId { raw }


instance FromJSON FieldProjKind where
  parseJSON v = case v of
  Object o | H.lookup "Adt" o /= Nothing -> do
  arr <- o .: "Adt"
  withArray "ProjAdt" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (ProjAdt v0 v1)) arr

    Object o | H.lookup "Tuple" o /= Nothing -> do
  v <- o .: "Tuple"
  ProjTuple <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON File where
  parseJSON = withObject "File" $ \o -> do
  name <- o .: "name"
    crateName <- o .: "crate_name"
    contents <- o .: "contents"
  pure File { name, crateName, contents }


instance FromJSON FileId where
  parseJSON v = parseJSON v


instance FromJSON FileName where
  parseJSON v = case v of
  Object o | H.lookup "Virtual" o /= Nothing -> do
  v <- o .: "Virtual"
  Virtual <$> parseJSON v

    Object o | H.lookup "Local" o /= Nothing -> do
  v <- o .: "Local"
  Local <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON FloatType where
  parseJSON v = case v of
  String "F16" -> pure F16
    String "F32" -> pure F32
    String "F64" -> pure F64
    String "F128" -> pure F128
  _ -> fail "Unknown variant"


instance FromJSON FloatValue where
  parseJSON = withObject "FloatValue" $ \o -> do
  floatValue <- o .: "value"
    floatTy <- o .: "ty"
  pure FloatValue { floatValue, floatTy }


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
  kind <- o .: "kind"
    generics <- o .: "generics"
  pure FnPtr { kind, generics }


instance FromJSON FnPtrKind where
  parseJSON v = case v of
  Object o | H.lookup "Fun" o /= Nothing -> do
  v <- o .: "Fun"
  FunId <$> parseJSON v

    Object o | H.lookup "Trait" o /= Nothing -> do
  arr <- o .: "Trait"
  withArray "TraitMethod" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (TraitMethod v0 v1 v2)) arr

  _ -> fail "Unknown variant"


instance FromJSON FunDeclId where
  parseJSON = withObject "FunDeclId" $ \o -> do
  raw <- o .: "_raw"
  pure FunDeclId { raw }


instance FromJSON FunDeclRef where
  parseJSON = withObject "FunDeclRef" $ \o -> do
  id <- o .: "id"
    generics <- o .: "generics"
  pure FunDeclRef { id, generics }


instance FromJSON FunId where
  parseJSON v = case v of
  Object o | H.lookup "Regular" o /= Nothing -> do
  v <- o .: "Regular"
  FRegular <$> parseJSON v

    Object o | H.lookup "Builtin" o /= Nothing -> do
  v <- o .: "Builtin"
  FBuiltin <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON FunSig where
  parseJSON = withObject "FunSig" $ \o -> do
  isUnsafe <- o .: "is_unsafe"
    generics <- o .: "generics"
    inputs <- o .: "inputs"
    output <- o .: "output"
  pure FunSig { isUnsafe, generics, inputs, output }


instance (FromJSON a0) => FromJSON GDeclarationGroup a0 where
  parseJSON v = case v of
  Object o | H.lookup "NonRec" o /= Nothing -> do
  v <- o .: "NonRec"
  NonRecGroup <$> parseJSON v

    Object o | H.lookup "Rec" o /= Nothing -> do
  v <- o .: "Rec"
  RecGroup <$> parseJSON v

  _ -> fail "Unknown variant"


instance (FromJSON a0) => FromJSON GexprBody a0 where
  parseJSON = withObject "GexprBody" $ \o -> do
  span <- o .: "span"
    locals <- o .: "locals"
    body <- o .: "body"
  pure GexprBody { span, locals, body }


instance FromJSON GenericArgs where
  parseJSON = withObject "GenericArgs" $ \o -> do
  regions <- o .: "regions"
    types <- o .: "types"
    constGenerics <- o .: "const_generics"
    traitRefs <- o .: "trait_refs"
  pure GenericArgs { regions, types, constGenerics, traitRefs }


instance FromJSON GenericParams where
  parseJSON = withObject "GenericParams" $ \o -> do
  regions <- o .: "regions"
    types <- o .: "types"
    constGenerics <- o .: "const_generics"
    traitClauses <- o .: "trait_clauses"
    regionsOutlive <- o .: "regions_outlive"
    typesOutlive <- o .: "types_outlive"
    traitTypeConstraints <- o .: "trait_type_constraints"
  pure GenericParams { regions, types, constGenerics, traitClauses, regionsOutlive, typesOutlive, traitTypeConstraints }


instance FromJSON GlobalDecl where
  parseJSON = withObject "GlobalDecl" $ \o -> do
  defId <- o .: "def_id"
    itemMeta <- o .: "item_meta"
    generics <- o .: "generics"
    ty <- o .: "ty"
    src <- o .: "src"
    globalKind <- o .: "global_kind"
    init <- o .: "init"
  pure GlobalDecl { defId, itemMeta, generics, ty, src, globalKind, init }


instance FromJSON GlobalDeclId where
  parseJSON = withObject "GlobalDeclId" $ \o -> do
  raw <- o .: "_raw"
  pure GlobalDeclId { raw }


instance FromJSON GlobalDeclRef where
  parseJSON = withObject "GlobalDeclRef" $ \o -> do
  id <- o .: "id"
    generics <- o .: "generics"
  pure GlobalDeclRef { id, generics }


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
  ImplElemTy <$> parseJSON v

    Object o | H.lookup "Trait" o /= Nothing -> do
  v <- o .: "Trait"
  ImplElemTrait <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON InlineAttr where
  parseJSON v = case v of
  String "Hint" -> pure Hint
    String "Never" -> pure Never
    String "Always" -> pure Always
  _ -> fail "Unknown variant"


instance FromJSON IntTy where
  parseJSON v = case v of
  String "Isize" -> pure Isize
    String "I8" -> pure I8
    String "I16" -> pure I16
    String "I32" -> pure I32
    String "I64" -> pure I64
    String "I128" -> pure I128
  _ -> fail "Unknown variant"


instance FromJSON IntegerType where
  parseJSON v = case v of
  Object o | H.lookup "Signed" o /= Nothing -> do
  v <- o .: "Signed"
  Signed <$> parseJSON v

    Object o | H.lookup "Unsigned" o /= Nothing -> do
  v <- o .: "Unsigned"
  Unsigned <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON ItemId where
  parseJSON v = case v of
  Object o | H.lookup "Type" o /= Nothing -> do
  v <- o .: "Type"
  IdType <$> parseJSON v

    Object o | H.lookup "Fun" o /= Nothing -> do
  v <- o .: "Fun"
  IdFun <$> parseJSON v

    Object o | H.lookup "Global" o /= Nothing -> do
  v <- o .: "Global"
  IdGlobal <$> parseJSON v

    Object o | H.lookup "TraitDecl" o /= Nothing -> do
  v <- o .: "TraitDecl"
  IdTraitDecl <$> parseJSON v

    Object o | H.lookup "TraitImpl" o /= Nothing -> do
  v <- o .: "TraitImpl"
  IdTraitImpl <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON ItemMeta where
  parseJSON = withObject "ItemMeta" $ \o -> do
  name <- o .: "name"
    span <- o .: "span"
    sourceText <- o .: "source_text"
    attrInfo <- o .: "attr_info"
    isLocal <- o .: "is_local"
    langItem <- o .: "lang_item"
  pure ItemMeta { name, span, sourceText, attrInfo, isLocal, langItem }


instance FromJSON ItemSource where
  parseJSON v = case v of
  String "TopLevel" -> pure TopLevelItem
    Object o | H.lookup "Closure" o /= Nothing -> do
  v <- o .: "Closure"
  ClosureItem <$> parseJSON v

    Object o | H.lookup "TraitDecl" o /= Nothing -> do
  arr <- o .: "TraitDecl"
  withArray "TraitDeclItem" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (TraitDeclItem v0 v1 v2)) arr

    Object o | H.lookup "TraitImpl" o /= Nothing -> do
  arr <- o .: "TraitImpl"
  withArray "TraitImplItem" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
        v3 <- parseJSON =<< arr .! 3
    pure (TraitImplItem v0 v1 v2 v3)) arr

    Object o | H.lookup "VTableTy" o /= Nothing -> do
  v <- o .: "VTableTy"
  VTableTyItem <$> parseJSON v

    Object o | H.lookup "VTableInstance" o /= Nothing -> do
  v <- o .: "VTableInstance"
  VTableInstanceItem <$> parseJSON v

    String "VTableMethodShim" -> pure VTableMethodShimItem
  _ -> fail "Unknown variant"


instance FromJSON Layout where
  parseJSON = withObject "Layout" $ \o -> do
  size <- o .: "size"
    align <- o .: "align"
    discriminantLayout <- o .: "discriminant_layout"
    uninhabited <- o .: "uninhabited"
    variantLayouts <- o .: "variant_layouts"
  pure Layout { size, align, discriminantLayout, uninhabited, variantLayouts }


instance FromJSON Literal where
  parseJSON v = case v of
  Object o | H.lookup "Scalar" o /= Nothing -> do
  v <- o .: "Scalar"
  VScalar <$> parseJSON v

    Object o | H.lookup "Float" o /= Nothing -> do
  v <- o .: "Float"
  VFloat <$> parseJSON v

    Object o | H.lookup "Bool" o /= Nothing -> do
  v <- o .: "Bool"
  VBool <$> parseJSON v

    Object o | H.lookup "Char" o /= Nothing -> do
  v <- o .: "Char"
  VChar <$> parseJSON v

    Object o | H.lookup "ByteStr" o /= Nothing -> do
  v <- o .: "ByteStr"
  VByteStr <$> parseJSON v

    Object o | H.lookup "Str" o /= Nothing -> do
  v <- o .: "Str"
  VStr <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON LiteralType where
  parseJSON v = case v of
  Object o | H.lookup "Int" o /= Nothing -> do
  v <- o .: "Int"
  TInt <$> parseJSON v

    Object o | H.lookup "UInt" o /= Nothing -> do
  v <- o .: "UInt"
  TuInt <$> parseJSON v

    Object o | H.lookup "Float" o /= Nothing -> do
  v <- o .: "Float"
  TFloat <$> parseJSON v

    String "Bool" -> pure TBool
    String "Char" -> pure TChar
  _ -> fail "Unknown variant"


instance FromJSON Loc where
  parseJSON = withObject "Loc" $ \o -> do
  line <- o .: "line"
    col <- o .: "col"
  pure Loc { line, col }


instance FromJSON Local where
  parseJSON = withObject "Local" $ \o -> do
  index <- o .: "index"
    name <- o .: "name"
    localTy <- o .: "ty"
  pure Local { index, name, localTy }


instance FromJSON LocalId where
  parseJSON = withObject "LocalId" $ \o -> do
  raw <- o .: "_raw"
  pure LocalId { raw }


instance FromJSON Locals where
  parseJSON = withObject "Locals" $ \o -> do
  argCount <- o .: "arg_count"
    locals <- o .: "locals"
  pure Locals { argCount, locals }


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
  parseJSON = withObject "Name" $ \o -> do
  name <- o .: "name"
  pure Name { name }


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


instance (FromJSON a0, FromJSON a1) => FromJSON OutlivesPred a0 a1 where
  parseJSON = withArray "OutlivesPred" $ \v -> do
  v0 <- parseJSON =<< v .! 0
    v1 <- parseJSON =<< v .! 1
  pure (OutlivesPred v0 v1)


instance FromJSON OverflowMode where
  parseJSON v = case v of
  String "Panic" -> pure OPanic
    String "UB" -> pure Oub
    String "Wrap" -> pure OWrap
  _ -> fail "Unknown variant"


instance FromJSON PathElem where
  parseJSON v = case v of
  Object o | H.lookup "Ident" o /= Nothing -> do
  arr <- o .: "Ident"
  withArray "PeIdent" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (PeIdent v0 v1)) arr

    Object o | H.lookup "Impl" o /= Nothing -> do
  v <- o .: "Impl"
  PeImpl <$> parseJSON v

    Object o | H.lookup "Instantiated" o /= Nothing -> do
  v <- o .: "Instantiated"
  PeInstantiated <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON Place where
  parseJSON = withObject "Place" $ \o -> do
  kind <- o .: "kind"
    ty <- o .: "ty"
  pure Place { kind, ty }


instance FromJSON PlaceKind where
  parseJSON v = case v of
  Object o | H.lookup "Local" o /= Nothing -> do
  v <- o .: "Local"
  PlaceLocal <$> parseJSON v

    Object o | H.lookup "Projection" o /= Nothing -> do
  arr <- o .: "Projection"
  withArray "PlaceProjection" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (PlaceProjection v0 v1)) arr

    Object o | H.lookup "Global" o /= Nothing -> do
  v <- o .: "Global"
  PlaceGlobal <$> parseJSON v

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
  String "Deref" -> pure Deref
    Object o | H.lookup "Field" o /= Nothing -> do
  arr <- o .: "Field"
  withArray "Field" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (Field v0 v1)) arr

    String "PtrMetadata" -> pure PtrMetadata
    Object o | H.lookup "Index" o /= Nothing -> do
  arr <- o .: "Index"
  withArray "ProjIndex" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (ProjIndex v0 v1)) arr

    Object o | H.lookup "Subslice" o /= Nothing -> do
  arr <- o .: "Subslice"
  withArray "Subslice" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (Subslice v0 v1 v2)) arr

  _ -> fail "Unknown variant"


instance FromJSON PtrMetadata where
  parseJSON v = case v of
  String "None" -> pure NoMetadata
    String "Length" -> pure Length
    Object o | H.lookup "VTable" o /= Nothing -> do
  v <- o .: "VTable"
  VTable <$> parseJSON v

    Object o | H.lookup "InheritFrom" o /= Nothing -> do
  v <- o .: "InheritFrom"
  InheritFrom <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON RawAttribute where
  parseJSON = withObject "RawAttribute" $ \o -> do
  path <- o .: "path"
    args <- o .: "args"
  pure RawAttribute { path, args }


instance FromJSON RefKind where
  parseJSON v = case v of
  String "Mut" -> pure RMut
    String "Shared" -> pure RShared
  _ -> fail "Unknown variant"


instance FromJSON Region where
  parseJSON v = case v of
  Object o | H.lookup "Var" o /= Nothing -> do
  v <- o .: "Var"
  RVar <$> parseJSON v

    String "Static" -> pure RStatic
    String "Erased" -> pure RErased
  _ -> fail "Unknown variant"


instance (FromJSON a0) => FromJSON RegionBinder a0 where
  parseJSON = withObject "RegionBinder" $ \o -> do
  binderRegions <- o .: "regions"
    binderValue <- o .: "skip_binder"
  pure RegionBinder { binderRegions, binderValue }


instance FromJSON RegionId where
  parseJSON = withObject "RegionId" $ \o -> do
  raw <- o .: "_raw"
  pure RegionId { raw }


instance FromJSON RegionParam where
  parseJSON = withObject "RegionParam" $ \o -> do
  index <- o .: "index"
    name <- o .: "name"
  pure RegionParam { index, name }


instance FromJSON ReprAlgorithm where
  parseJSON v = case v of
  String "Rust" -> pure Rust
    String "C" -> pure C
  _ -> fail "Unknown variant"


instance FromJSON ReprOptions where
  parseJSON = withObject "ReprOptions" $ \o -> do
  reprAlgo <- o .: "repr_algo"
    alignModif <- o .: "align_modif"
    transparent <- o .: "transparent"
    explicitDiscrType <- o .: "explicit_discr_type"
  pure ReprOptions { reprAlgo, alignModif, transparent, explicitDiscrType }


instance FromJSON Rvalue where
  parseJSON v = case v of
  Object o | H.lookup "Use" o /= Nothing -> do
  v <- o .: "Use"
  Use <$> parseJSON v

    Object o | H.lookup "Ref" o /= Nothing -> do
  arr <- o .: "Ref"
  withArray "RvRef" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (RvRef v0 v1 v2)) arr

    Object o | H.lookup "RawPtr" o /= Nothing -> do
  arr <- o .: "RawPtr"
  withArray "RawPtr" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (RawPtr v0 v1 v2)) arr

    Object o | H.lookup "BinaryOp" o /= Nothing -> do
  arr <- o .: "BinaryOp"
  withArray "BinaryOp" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (BinaryOp v0 v1 v2)) arr

    Object o | H.lookup "UnaryOp" o /= Nothing -> do
  arr <- o .: "UnaryOp"
  withArray "UnaryOp" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (UnaryOp v0 v1)) arr

    Object o | H.lookup "NullaryOp" o /= Nothing -> do
  arr <- o .: "NullaryOp"
  withArray "NullaryOp" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (NullaryOp v0 v1)) arr

    Object o | H.lookup "Discriminant" o /= Nothing -> do
  v <- o .: "Discriminant"
  Discriminant <$> parseJSON v

    Object o | H.lookup "Aggregate" o /= Nothing -> do
  arr <- o .: "Aggregate"
  withArray "Aggregate" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (Aggregate v0 v1)) arr

    Object o | H.lookup "Len" o /= Nothing -> do
  arr <- o .: "Len"
  withArray "Len" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (Len v0 v1 v2)) arr

    Object o | H.lookup "Repeat" o /= Nothing -> do
  arr <- o .: "Repeat"
  withArray "Repeat" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (Repeat v0 v1 v2)) arr

    Object o | H.lookup "ShallowInitBox" o /= Nothing -> do
  arr <- o .: "ShallowInitBox"
  withArray "ShallowInitBox" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (ShallowInitBox v0 v1)) arr

  _ -> fail "Unknown variant"


instance FromJSON ScalarValue where
  parseJSON v = case v of
  Object o | H.lookup "Unsigned" o /= Nothing -> do
  arr <- o .: "Unsigned"
  withArray "UnsignedScalar" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (UnsignedScalar v0 v1)) arr

    Object o | H.lookup "Signed" o /= Nothing -> do
  arr <- o .: "Signed"
  withArray "SignedScalar" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (SignedScalar v0 v1)) arr

  _ -> fail "Unknown variant"


instance FromJSON Span where
  parseJSON = withObject "Span" $ \o -> do
  data_ <- o .: "data"
    generatedFromSpan <- o .: "generated_from_span"
  pure Span { data_, generatedFromSpan }


instance FromJSON SpanData where
  parseJSON = withObject "SpanData" $ \o -> do
  file <- o .: "file_id"
    begLoc <- o .: "beg"
    endLoc <- o .: "end"
  pure SpanData { file, begLoc, endLoc }


instance FromJSON TagEncoding where
  parseJSON v = case v of
  String "Direct" -> pure Direct
    Object o | H.lookup "Niche" o /= Nothing -> do
  v <- o .: "Niche"
  Niche <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON TargetInfo where
  parseJSON = withObject "TargetInfo" $ \o -> do
  targetPointerSize <- o .: "target_pointer_size"
    isLittleEndian <- o .: "is_little_endian"
  pure TargetInfo { targetPointerSize, isLittleEndian }


instance FromJSON TraitAssocConst where
  parseJSON = withObject "TraitAssocConst" $ \o -> do
  name <- o .: "name"
    ty <- o .: "ty"
    default <- o .: "default"
  pure TraitAssocConst { name, ty, default }


instance FromJSON TraitAssocTy where
  parseJSON = withObject "TraitAssocTy" $ \o -> do
  name <- o .: "name"
    default <- o .: "default"
    impliedClauses <- o .: "implied_clauses"
  pure TraitAssocTy { name, default, impliedClauses }


instance FromJSON TraitAssocTyImpl where
  parseJSON = withObject "TraitAssocTyImpl" $ \o -> do
  value <- o .: "value"
  pure TraitAssocTyImpl { value }


instance FromJSON TraitClauseId where
  parseJSON = withObject "TraitClauseId" $ \o -> do
  raw <- o .: "_raw"
  pure TraitClauseId { raw }


instance FromJSON TraitDecl where
  parseJSON = withObject "TraitDecl" $ \o -> do
  defId <- o .: "def_id"
    itemMeta <- o .: "item_meta"
    generics <- o .: "generics"
    impliedClauses <- o .: "implied_clauses"
    consts <- o .: "consts"
    types <- o .: "types"
    methods <- o .: "methods"
    vtable <- o .: "vtable"
  pure TraitDecl { defId, itemMeta, generics, impliedClauses, consts, types, methods, vtable }


instance FromJSON TraitDeclId where
  parseJSON = withObject "TraitDeclId" $ \o -> do
  raw <- o .: "_raw"
  pure TraitDeclId { raw }


instance FromJSON TraitDeclRef where
  parseJSON = withObject "TraitDeclRef" $ \o -> do
  id <- o .: "id"
    generics <- o .: "generics"
  pure TraitDeclRef { id, generics }


instance FromJSON TraitImpl where
  parseJSON = withObject "TraitImpl" $ \o -> do
  defId <- o .: "def_id"
    itemMeta <- o .: "item_meta"
    implTrait <- o .: "impl_trait"
    generics <- o .: "generics"
    impliedTraitRefs <- o .: "implied_trait_refs"
    consts <- o .: "consts"
    types <- o .: "types"
    methods <- o .: "methods"
    vtable <- o .: "vtable"
  pure TraitImpl { defId, itemMeta, implTrait, generics, impliedTraitRefs, consts, types, methods, vtable }


instance FromJSON TraitImplId where
  parseJSON = withObject "TraitImplId" $ \o -> do
  raw <- o .: "_raw"
  pure TraitImplId { raw }


instance FromJSON TraitImplRef where
  parseJSON = withObject "TraitImplRef" $ \o -> do
  id <- o .: "id"
    generics <- o .: "generics"
  pure TraitImplRef { id, generics }


instance FromJSON TraitItemName where
  parseJSON = withArray "TraitItemName" $ \v -> do
  v0 <- parseJSON =<< v .! 0
  pure (TraitItemName v0)


instance FromJSON TraitMethod where
  parseJSON = withObject "TraitMethod" $ \o -> do
  name <- o .: "name"
    item <- o .: "item"
  pure TraitMethod { name, item }


instance FromJSON TraitParam where
  parseJSON = withObject "TraitParam" $ \o -> do
  clauseId <- o .: "clause_id"
    span <- o .: "span"
    trait <- o .: "trait_"
  pure TraitParam { clauseId, span, trait }


instance FromJSON TraitRef where
  parseJSON = withObject "TraitRef" $ \o -> do
  kind <- o .: "kind"
    traitDeclRef <- o .: "trait_decl_ref"
  pure TraitRef { kind, traitDeclRef }


instance FromJSON TraitRefKind where
  parseJSON v = case v of
  Object o | H.lookup "TraitImpl" o /= Nothing -> do
  v <- o .: "TraitImpl"
  TraitImpl <$> parseJSON v

    Object o | H.lookup "Clause" o /= Nothing -> do
  v <- o .: "Clause"
  Clause <$> parseJSON v

    Object o | H.lookup "ParentClause" o /= Nothing -> do
  arr <- o .: "ParentClause"
  withArray "ParentClause" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (ParentClause v0 v1)) arr

    Object o | H.lookup "ItemClause" o /= Nothing -> do
  arr <- o .: "ItemClause"
  withArray "ItemClause" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (ItemClause v0 v1 v2)) arr

    String "SelfId" -> pure Self
    Object o | H.lookup "BuiltinOrAuto" o /= Nothing -> do
  arr <- o .: "BuiltinOrAuto"
  withArray "BuiltinOrAuto" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (BuiltinOrAuto v0 v1 v2)) arr

    String "Dyn" -> pure Dyn
    Object o | H.lookup "Unknown" o /= Nothing -> do
  v <- o .: "Unknown"
  UnknownTrait <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON TraitTypeConstraint where
  parseJSON = withObject "TraitTypeConstraint" $ \o -> do
  traitRef <- o .: "trait_ref"
    typeName <- o .: "type_name"
    ty <- o .: "ty"
  pure TraitTypeConstraint { traitRef, typeName, ty }


instance FromJSON TraitTypeConstraintId where
  parseJSON = withObject "TraitTypeConstraintId" $ \o -> do
  raw <- o .: "_raw"
  pure TraitTypeConstraintId { raw }


instance FromJSON Ty where
  parseJSON v = case v of
  Object o | H.lookup "Adt" o /= Nothing -> do
  v <- o .: "Adt"
  TAdt <$> parseJSON v

    Object o | H.lookup "TypeVar" o /= Nothing -> do
  v <- o .: "TypeVar"
  TVar <$> parseJSON v

    Object o | H.lookup "Literal" o /= Nothing -> do
  v <- o .: "Literal"
  TLiteral <$> parseJSON v

    String "Never" -> pure TNever
    Object o | H.lookup "Ref" o /= Nothing -> do
  arr <- o .: "Ref"
  withArray "TRef" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
        v2 <- parseJSON =<< arr .! 2
    pure (TRef v0 v1 v2)) arr

    Object o | H.lookup "RawPtr" o /= Nothing -> do
  arr <- o .: "RawPtr"
  withArray "TRawPtr" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (TRawPtr v0 v1)) arr

    Object o | H.lookup "TraitType" o /= Nothing -> do
  arr <- o .: "TraitType"
  withArray "TTraitType" (\v -> do
    v0 <- parseJSON =<< arr .! 0
        v1 <- parseJSON =<< arr .! 1
    pure (TTraitType v0 v1)) arr

    Object o | H.lookup "DynTrait" o /= Nothing -> do
  v <- o .: "DynTrait"
  TDynTrait <$> parseJSON v

    Object o | H.lookup "FnPtr" o /= Nothing -> do
  v <- o .: "FnPtr"
  TFnPtr <$> parseJSON v

    Object o | H.lookup "FnDef" o /= Nothing -> do
  v <- o .: "FnDef"
  TFnDef <$> parseJSON v

    Object o | H.lookup "PtrMetadata" o /= Nothing -> do
  v <- o .: "PtrMetadata"
  TPtrMetadata <$> parseJSON v

    Object o | H.lookup "Error" o /= Nothing -> do
  v <- o .: "Error"
  TError <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON TypeDecl where
  parseJSON = withObject "TypeDecl" $ \o -> do
  defId <- o .: "def_id"
    itemMeta <- o .: "item_meta"
    generics <- o .: "generics"
    src <- o .: "src"
    kind <- o .: "kind"
    layout <- o .: "layout"
    ptrMetadata <- o .: "ptr_metadata"
    repr <- o .: "repr"
  pure TypeDecl { defId, itemMeta, generics, src, kind, layout, ptrMetadata, repr }


instance FromJSON TypeDeclId where
  parseJSON = withObject "TypeDeclId" $ \o -> do
  raw <- o .: "_raw"
  pure TypeDeclId { raw }


instance FromJSON TypeDeclKind where
  parseJSON v = case v of
  Object o | H.lookup "Struct" o /= Nothing -> do
  v <- o .: "Struct"
  Struct <$> parseJSON v

    Object o | H.lookup "Enum" o /= Nothing -> do
  v <- o .: "Enum"
  Enum <$> parseJSON v

    Object o | H.lookup "Union" o /= Nothing -> do
  v <- o .: "Union"
  Union <$> parseJSON v

    String "Opaque" -> pure Opaque
    Object o | H.lookup "Alias" o /= Nothing -> do
  v <- o .: "Alias"
  Alias <$> parseJSON v

    Object o | H.lookup "Error" o /= Nothing -> do
  v <- o .: "Error"
  TDeclError <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON TypeDeclRef where
  parseJSON = withObject "TypeDeclRef" $ \o -> do
  id <- o .: "id"
    generics <- o .: "generics"
  pure TypeDeclRef { id, generics }


instance FromJSON TypeId where
  parseJSON v = case v of
  Object o | H.lookup "Adt" o /= Nothing -> do
  v <- o .: "Adt"
  TAdtId <$> parseJSON v

    String "Tuple" -> pure TTuple
    Object o | H.lookup "Builtin" o /= Nothing -> do
  v <- o .: "Builtin"
  TBuiltin <$> parseJSON v

  _ -> fail "Unknown variant"


instance FromJSON TypeParam where
  parseJSON = withObject "TypeParam" $ \o -> do
  index <- o .: "index"
    name <- o .: "name"
  pure TypeParam { index, name }


instance FromJSON TypeVarId where
  parseJSON = withObject "TypeVarId" $ \o -> do
  raw <- o .: "_raw"
  pure TypeVarId { raw }


instance FromJSON UIntTy where
  parseJSON v = case v of
  String "Usize" -> pure Usize
    String "U8" -> pure U8
    String "U16" -> pure U16
    String "U32" -> pure U32
    String "U64" -> pure U64
    String "U128" -> pure U128
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


instance FromJSON Variant where
  parseJSON = withObject "Variant" $ \o -> do
  span <- o .: "span"
    attrInfo <- o .: "attr_info"
    variantName <- o .: "name"
    fields <- o .: "fields"
    discriminant <- o .: "discriminant"
  pure Variant { span, attrInfo, variantName, fields, discriminant }


instance FromJSON VariantId where
  parseJSON = withObject "VariantId" $ \o -> do
  raw <- o .: "_raw"
  pure VariantId { raw }


instance FromJSON VariantLayout where
  parseJSON = withObject "VariantLayout" $ \o -> do
  fieldOffsets <- o .: "field_offsets"
    uninhabited <- o .: "uninhabited"
    tag <- o .: "tag"
  pure VariantLayout { fieldOffsets, uninhabited, tag }


instance (FromJSON a0, FromJSON a1) => FromJSON Vector a0 a1 where
  parseJSON = fmap catMaybes . parseJSON

