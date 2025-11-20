{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Types.hs`
by hand. Edit `templates/Types.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Types where

import Data.Aeson (FromJSON(..), Value(..), withObject, withArray, (.:), (.!=))
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import qualified Generated_Meta as M
import {-# SOURCE #-} qualified Generated_Values as Val
import {-# SOURCE #-} qualified Generated_Expressions as E
import {-# SOURCE #-} qualified Generated_GAst as G

-- Re-export commonly used types for convenience
type Vector = M.Vector

-- Manually defined type aliases and newtypes
-- TraitTypeConstraintId is a newtype wrapper around Int
newtype TraitTypeConstraintId = TraitTypeConstraintId { traittypeconstraintidRaw :: Int }
  deriving (Show, Eq, Ord)

instance FromJSON TraitTypeConstraintId where
  parseJSON v = TraitTypeConstraintId <$> parseJSON v

-- | (U)LLBC is a language with side-effects: a statement may abort in a way that isn't tracked by
-- | control-flow. The two kinds of abort are:
-- | - Panic (may unwind or not depending on compilation setting);
-- | - Undefined behavior:
data AbortKind = Panic (Maybe Name)
  | UndefinedBehavior
  | UnwindTerminate
  deriving (Show, Eq, Ord)

-- | Describes modifiers to the alignment and packing of the corresponding type.
-- | Represents `repr(align(n))` and `repr(packed(n))`.
data AlignmentModifier = Align Int
  | Pack Int
  deriving (Show, Eq, Ord)

-- | Additional information for closures.
data ClosureInfo = ClosureInfo
  { closureinfoKind :: ClosureKind
  ,   -- | The `FnOnce` implementation of this closure -- always exists.
  closureinfoFnOnceImpl :: (RegionBinder TraitImplRef)
  ,   -- | The `FnMut` implementation of this closure, if any.
  closureinfoFnMutImpl :: Maybe (RegionBinder TraitImplRef)
  ,   -- | The `Fn` implementation of this closure, if any.
  closureinfoFnImpl :: Maybe (RegionBinder TraitImplRef)
  ,   -- | The signature of the function that this closure represents.
  closureinfoSignature :: (RegionBinder ([Ty], Ty))
  }
  deriving (Show, Eq, Ord)

data ClosureKind = Fn
  | FnMut
  | FnOnce
  deriving (Show, Eq, Ord)

-- | Layout of the discriminant.
-- | Describes the offset of the discriminant field as well as its encoding
-- | as `tag` in memory.
data DiscriminantLayout = DiscriminantLayout
  {   -- | The offset of the discriminant in bytes.
  discriminantlayoutOffset :: Int
  ,   -- | The representation type of the discriminant.
  discriminantlayoutTagTy :: IntegerType
  ,   -- | How the tag is encoding in memory.
  discriminantlayoutEncoding :: TagEncoding
  }
  deriving (Show, Eq, Ord)

data Field = Field
  { fieldSpan :: M.Span
  , fieldAttrInfo :: M.AttrInfo
  , fieldFieldName :: Maybe String
  , fieldFieldTy :: Ty
  }
  deriving (Show, Eq, Ord)

data FieldId = FieldId
  { fieldidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Reference to a function declaration.
data FunDeclRef = FunDeclRef
  { fundeclrefId :: G.FunDeclId
  ,   -- | Generic arguments passed to the function.
  fundeclrefGenerics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

-- | Reference to a global declaration.
data GlobalDeclRef = GlobalDeclRef
  { globaldeclrefId :: G.GlobalDeclId
  , globaldeclrefGenerics :: GenericArgs
  }
  deriving (Show, Eq, Ord)

data IntegerType = Signed IntTy
  | Unsigned UIntTy
  deriving (Show, Eq, Ord)

-- | The id of a translated item.
data ItemId = IdType G.TypeDeclId
  | IdFun G.FunDeclId
  | IdGlobal G.GlobalDeclId
  | IdTraitDecl G.TraitDeclId
  | IdTraitImpl G.TraitImplId
  deriving (Show, Eq, Ord)

-- | Item kind: whether this function/const is part of a trait declaration, trait implementation, or
-- | neither.
-- | 
-- | Example:
-- | ```text
-- | trait Foo {
-- |     fn bar(x : u32) -> u32; // trait item decl without default
-- | 
-- |     fn baz(x : bool) -> bool { x } // trait item decl with default
-- | }
-- | 
-- | impl Foo for ... {
-- |     fn bar(x : u32) -> u32 { x } // trait item implementation
-- | }
-- | 
-- | fn test(...) { ... } // regular
-- | 
-- | impl Type {
-- |     fn test(...) { ... } // regular
-- | }
-- | ```
data ItemSource = TopLevelItem
  | ClosureItem ClosureInfo
  | TraitDeclItem TraitDeclRef G.TraitItemName Bool
  | TraitImplItem TraitImplRef TraitDeclRef G.TraitItemName Bool
  | VTableTyItem DynPredicate
  | VTableInstanceItem TraitImplRef
  | VTableMethodShimItem
  deriving (Show, Eq, Ord)

-- | Simplified type layout information.
-- | 
-- | Does not include information about niches.
-- | If the type does not have a fully known layout (e.g. it is ?Sized)
-- | some of the layout parts are not available.
data Layout = Layout
  {   -- | The size of the type in bytes.
  layoutSize :: Maybe Int
  ,   -- | The alignment, in bytes.
  layoutAlign :: Maybe Int
  ,   -- | The discriminant's layout, if any. Only relevant for types with multiple variants.
  layoutDiscriminantLayout :: Maybe DiscriminantLayout
  ,   -- | Whether the type is uninhabited, i.e. has any valid value at all.
  -- | Note that uninhabited types can have arbitrary layouts: `(u32, !)` has space for the `u32`
  -- | and `enum E2 { A, B(!), C(i32, !) }` may have space for a discriminant.
  layoutUninhabited :: Bool
  ,   -- | Map from `VariantId` to the corresponding field layouts. Structs are modeled as having
  -- | exactly one variant, unions as having no variant.
  layoutVariantLayouts :: (M.Vector VariantId VariantLayout)
  }
  deriving (Show, Eq, Ord)

-- | The metadata stored in a pointer. That's the information stored in pointers alongside
-- | their address. It's empty for `Sized` types, and interesting for unsized
-- | aka dynamically-sized types.
data PtrMetadata = NoMetadata
  | Length
  | VTable TypeDeclRef
  | InheritFrom Ty
  deriving (Show, Eq, Ord)

-- | Describes which layout algorithm is used for representing the corresponding type.
-- | Depends on the `#[repr(...)]` used.
data ReprAlgorithm = Rust
  | C
  deriving (Show, Eq, Ord)

-- | The representation options as annotated by the user.
-- | 
-- | NOTE: This does not include less common/unstable representations such as `#[repr(simd)]`
-- | or the compiler internal `#[repr(linear)]`. Similarly, enum discriminant representations
-- | are encoded in [`Variant::discriminant`] and [`DiscriminantLayout`] instead.
-- | This only stores whether the discriminant type was derived from an explicit annotation.
data ReprOptions = ReprOptions
  { reproptionsReprAlgo :: ReprAlgorithm
  , reproptionsAlignModif :: Maybe AlignmentModifier
  , reproptionsTransparent :: Bool
  , reproptionsExplicitDiscrType :: Bool
  }
  deriving (Show, Eq, Ord)

-- | Describes how we represent the active enum variant in memory.
data TagEncoding = Direct
  | Niche VariantId
  deriving (Show, Eq, Ord)

-- | A type declaration.
-- | 
-- | Types can be opaque or transparent.
-- | 
-- | Transparent types are local types not marked as opaque.
-- | Opaque types are the others: local types marked as opaque, and non-local
-- | types (coming from external dependencies).
-- | 
-- | In case the type is transparent, the declaration also contains the
-- | type definition (see [TypeDeclKind]).
-- | 
-- | A type can only be an ADT (structure or enumeration), as type aliases are
-- | inlined in MIR.
data TypeDecl = TypeDecl
  { typedeclDefId :: G.TypeDeclId
  ,   -- | Meta information associated with the item.
  typedeclItemMeta :: M.ItemMeta
  , typedeclGenerics :: GenericParams
  ,   -- | The context of the type: distinguishes top-level items from closure-related items.
  typedeclSrc :: ItemSource
  ,   -- | The type kind: enum, struct, or opaque.
  typedeclKind :: TypeDeclKind
  ,   -- | The layout of the type. Information may be partial because of generics or dynamically-
  -- | sized types. If rustc cannot compute a layout, it is `None`.
  typedeclLayout :: Maybe Layout
  ,   -- | The metadata associated with a pointer to the type.
  typedeclPtrMetadata :: PtrMetadata
  ,   -- | The representation options of this type declaration as annotated by the user.
  -- | Is `None` for foreign type declarations.
  typedeclRepr :: Maybe ReprOptions
  }
  deriving (Show, Eq, Ord)

data TypeDeclKind = Struct ((M.Vector FieldId Field))
  | Enum ((M.Vector VariantId Variant))
  | Union ((M.Vector FieldId Field))
  | Opaque
  | Alias Ty
  | TDeclError String
  deriving (Show, Eq, Ord)

data Variant = Variant
  { variantSpan :: M.Span
  , variantAttrInfo :: M.AttrInfo
  , variantVariantName :: String
  , variantFields :: (M.Vector FieldId Field)
  ,   -- | The discriminant value outputted by `std::mem::discriminant` for this variant.
  -- | This can be different than the discriminant stored in memory (called `tag`).
  -- | That one is described by [`DiscriminantLayout`] and [`TagEncoding`].
  variantDiscriminant :: Val.Literal
  }
  deriving (Show, Eq, Ord)

data VariantId = VariantId
  { variantidRaw :: Int
  }
  deriving (Show, Eq, Ord)

-- | Simplified layout of a single variant.
-- | 
-- | Maps fields to their offset within the layout.
data VariantLayout = VariantLayout
  {   -- | The offset of each field.
  variantlayoutFieldOffsets :: (M.Vector FieldId Int)
  ,   -- | Whether the variant is uninhabited, i.e. has any valid possible value.
  -- | Note that uninhabited types can have arbitrary layouts.
  variantlayoutUninhabited :: Bool
  ,   -- | The memory representation of the discriminant corresponding to this
  -- | variant. It must be of the same type as the corresponding [`DiscriminantLayout::tag_ty`].
  -- | 
  -- | If it's `None`, then this variant is either:
  -- | - the untagged variant (cf. [`TagEncoding::Niche::untagged_variant`]) of a niched enum;
  -- | - the single variant of a struct;
  -- | - uninhabited.
  variantlayoutTag :: Maybe Val.ScalarValue
  }
  deriving (Show, Eq, Ord)

instance FromJSON AbortKind where
  parseJSON v = case v of
    Object o | H.lookup "Panic" o /= Nothing -> do
      v <- o .: "Panic"
      Panic <$> parseJSON v
    String "UndefinedBehavior" -> pure UndefinedBehavior
    String "UnwindTerminate" -> pure UnwindTerminate
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
    String "Fn" -> pure Fn
    String "FnMut" -> pure FnMut
    String "FnOnce" -> pure FnOnce
    _ -> fail "Unknown variant"


instance FromJSON DiscriminantLayout where
  parseJSON = withObject "DiscriminantLayout" $ \o -> do
    discriminantlayoutOffset <- o .: "offset"
    discriminantlayoutTagTy <- o .: "tag_ty"
    discriminantlayoutEncoding <- o .: "encoding"
    pure (DiscriminantLayout discriminantlayoutOffset discriminantlayoutTagTy discriminantlayoutEncoding)


instance FromJSON Field where
  parseJSON = withObject "Field" $ \o -> do
    fieldSpan <- o .: "span"
    fieldAttrInfo <- o .: "attr_info"
    fieldFieldName <- o .: "name"
    fieldFieldTy <- o .: "ty"
    pure (Field fieldSpan fieldAttrInfo fieldFieldName fieldFieldTy)


instance FromJSON FieldId where
  parseJSON = fmap FieldId . parseJSON


instance FromJSON FunDeclRef where
  parseJSON = withObject "FunDeclRef" $ \o -> do
    fundeclrefId <- o .: "id"
    fundeclrefGenerics <- o .: "generics"
    pure (FunDeclRef fundeclrefId fundeclrefGenerics)


instance FromJSON GlobalDeclRef where
  parseJSON = withObject "GlobalDeclRef" $ \o -> do
    globaldeclrefId <- o .: "id"
    globaldeclrefGenerics <- o .: "generics"
    pure (GlobalDeclRef globaldeclrefId globaldeclrefGenerics)


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


instance FromJSON ItemSource where
  parseJSON v = case v of
    String "TopLevel" -> pure TopLevelItem
    Object o | H.lookup "Closure" o /= Nothing -> do
      obj <- o .: "Closure"
      info <- obj .: "info"
      pure (ClosureItem info)
    Object o | H.lookup "TraitDecl" o /= Nothing -> do
      obj <- o .: "TraitDecl"
      traitRef <- obj .: "trait_ref"
      itemName <- obj .: "item_name"
      hasDefault <- obj .: "has_default"
      pure (TraitDeclItem traitRef itemName hasDefault)
    Object o | H.lookup "TraitImpl" o /= Nothing -> do
      obj <- o .: "TraitImpl"
      implRef <- obj .: "impl_ref"
      traitRef <- obj .: "trait_ref"
      itemName <- obj .: "item_name"
      reusesDefault <- obj .: "reuses_default"
      pure (TraitImplItem implRef traitRef itemName reusesDefault)
    Object o | H.lookup "VTableTy" o /= Nothing -> do
      obj <- o .: "VTableTy"
      dynPredicate <- obj .: "dyn_predicate"
      pure (VTableTyItem dynPredicate)
    Object o | H.lookup "VTableInstance" o /= Nothing -> do
      obj <- o .: "VTableInstance"
      implRef <- obj .: "impl_ref"
      pure (VTableInstanceItem implRef)
    String "VTableMethodShim" -> pure VTableMethodShimItem
    _ -> fail "Unknown variant"


instance FromJSON Layout where
  parseJSON = withObject "Layout" $ \o -> do
    layoutSize <- o .: "size"
    layoutAlign <- o .: "align"
    layoutDiscriminantLayout <- o .: "discriminant_layout"
    layoutUninhabited <- o .: "uninhabited"
    layoutVariantLayouts <- o .: "variant_layouts"
    pure (Layout layoutSize layoutAlign layoutDiscriminantLayout layoutUninhabited layoutVariantLayouts)


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


instance FromJSON ReprAlgorithm where
  parseJSON v = case v of
    String "Rust" -> pure Rust
    String "C" -> pure C
    _ -> fail "Unknown variant"


instance FromJSON ReprOptions where
  parseJSON = withObject "ReprOptions" $ \o -> do
    reproptionsReprAlgo <- o .: "repr_algo"
    reproptionsAlignModif <- o .: "align_modif"
    reproptionsTransparent <- o .: "transparent"
    reproptionsExplicitDiscrType <- o .: "explicit_discr_type"
    pure (ReprOptions reproptionsReprAlgo reproptionsAlignModif reproptionsTransparent reproptionsExplicitDiscrType)


instance FromJSON TagEncoding where
  parseJSON v = case v of
    String "Direct" -> pure Direct
    Object o | H.lookup "Niche" o /= Nothing -> do
      obj <- o .: "Niche"
      untaggedVariant <- obj .: "untagged_variant"
      pure (Niche untaggedVariant)
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

