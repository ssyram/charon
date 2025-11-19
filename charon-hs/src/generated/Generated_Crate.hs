{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Crate.hs`
by hand. Edit `templates/Crate.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Crate where

import Data.Aeson
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import GHC.Exts (RandomState)
import Data.Text (Text, unpack)
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions
import qualified Generated_GAst as G
import Generated_GAst hiding (Call, CopyNonOverlapping)
import qualified Generated_LlbcAst as L
import qualified Generated_UllbcAst as U

-- Manually define Body with properly qualified types
-- The auto-generated version has ambiguous Block/BlockId references
data Body = Unstructured (GexprBody U.Blocks)
  | Structured (GexprBody L.Block)
  | TraitMethodWithoutDefault
  | Opaque
  | Missing
  | Error Error
  deriving (Show, Eq, Ord)

-- Manually defined FromJSON instance for Body
instance FromJSON Body where
  parseJSON v = case v of
    Object o | H.lookup "Unstructured" o /= Nothing -> do
      v <- o .: "Unstructured"
      Unstructured <$> parseJSON v
    Object o | H.lookup "Structured" o /= Nothing -> do
      v <- o .: "Structured"
      Structured <$> parseJSON v
    String "TraitMethodWithoutDefault" -> pure TraitMethodWithoutDefault
    String "Opaque" -> pure Opaque
    String "Missing" -> pure Missing
    Object o | H.lookup "Error" o /= Nothing -> do
      v <- o .: "Error"
      Error <$> parseJSON v
    _ -> fail "Unknown variant"

-- | Common error used during the translation.
data Error = Error
  { errorSpan :: Span
  , errorMsg :: String
  }
  deriving (Show, Eq, Ord)

-- | A function definition
data FunDecl = FunDecl
  { fundeclDefId :: FunDeclId
  ,   -- | The meta data associated with the declaration.
  fundeclItemMeta :: ItemMeta
  ,   -- | The signature contains the inputs/output types *with* non-erased regions.
  -- | It also contains the list of region and type parameters.
  fundeclSignature :: FunSig
  ,   -- | The function kind: "regular" function, trait method declaration, etc.
  fundeclSrc :: ItemSource
  ,   -- | Whether this function is in fact the body of a constant/static that we turned into an
  -- | initializer function.
  fundeclIsGlobalInitializer :: Maybe GlobalDeclId
  ,   -- | The function body, unless the function is opaque.
  -- | Opaque functions are: external functions, or local functions tagged
  -- | as opaque.
  fundeclBody :: Body
  }
  deriving (Show, Eq, Ord)

-- | The data of a translated crate.
data TranslatedCrate = TranslatedCrate
  {   -- | The name of the crate.
  translatedcrateCrateName :: String
  ,   -- | The options used when calling Charon. It is useful for the applications
  -- | which consumed the serialized code, to check that Charon was called with
  -- | the proper options.
  translatedcrateOptions :: CliOptions
  ,   -- | Information about the target platform for which rustc is called on for the crate.
  translatedcrateTargetInformation :: TargetInfo
  ,   -- | The names of all registered items. Available so we can know the names even of items that
  -- | failed to translate.
  -- | Invariant: after translation, any existing `ItemId` must have an associated name, even
  -- | if the corresponding item wasn't translated.
  translatedcrateItemNames :: (HashMap ItemId Name RandomState)
  ,   -- | Short names, for items whose last PathElem is unique.
  translatedcrateShortNames :: (HashMap ItemId Name RandomState)
  ,   -- | The translated files.
  translatedcrateFiles :: (Vector FileId File)
  ,   -- | The translated type definitions
  translatedcrateTypeDecls :: (Vector TypeDeclId TypeDecl)
  ,   -- | The translated function definitions
  translatedcrateFunDecls :: (Vector FunDeclId FunDecl)
  ,   -- | The translated global definitions
  translatedcrateGlobalDecls :: (Vector GlobalDeclId GlobalDecl)
  ,   -- | The translated trait declarations
  translatedcrateTraitDecls :: (Vector TraitDeclId TraitDecl)
  ,   -- | The translated trait declarations
  translatedcrateTraitImpls :: (Vector TraitImplId TraitImpl)
  ,   -- | A `const UNIT: () = ();` used whenever we make a thin pointer/reference to avoid creating a
  -- | local `let unit = ();` variable. It is always `Some`.
  translatedcrateUnitMetadata :: Maybe GlobalDeclRef
  ,   -- | The re-ordered groups of declarations, initialized as empty.
  translatedcrateOrderedDecls :: Maybe [DeclarationGroup]
  }
  deriving (Show, Eq, Ord)

-- Wrapper type for the top-level LLBC file structure
data LlbcFile = LlbcFile
  { llbcfileCharonVersion :: String
  , llbcfileTranslated :: TranslatedCrate
  }
  deriving (Show, Eq, Ord)

instance FromJSON LlbcFile where
  parseJSON = withObject "LlbcFile" $ \o -> do
    charonVersion <- o .: "charon_version"
    translated <- o .: "translated"
    pure $ LlbcFile charonVersion translated

instance FromJSON Error where
  parseJSON = withObject "Error" $ \o -> do
    errorSpan <- o .: "span"
    errorMsg <- o .: "msg"
    pure (Error errorSpan errorMsg)


instance FromJSON FunDecl where
  parseJSON = withObject "FunDecl" $ \o -> do
    fundeclDefId <- o .: "def_id"
    fundeclItemMeta <- o .: "item_meta"
    fundeclSignature <- o .: "signature"
    fundeclSrc <- o .: "src"
    fundeclIsGlobalInitializer <- o .: "is_global_initializer"
    fundeclBody <- o .: "body"
    pure (FunDecl fundeclDefId fundeclItemMeta fundeclSignature fundeclSrc fundeclIsGlobalInitializer fundeclBody)


instance FromJSON TranslatedCrate where
  parseJSON = withObject "TranslatedCrate" $ \o -> do
    translatedcrateCrateName <- o .: "crate_name"
    translatedcrateOptions <- o .: "options"
    translatedcrateTargetInformation <- o .: "target_information"
    translatedcrateItemNames <- o .: "item_names"
    translatedcrateShortNames <- o .: "short_names"
    translatedcrateFiles <- o .: "files"
    translatedcrateTypeDecls <- o .: "type_decls"
    translatedcrateFunDecls <- o .: "fun_decls"
    translatedcrateGlobalDecls <- o .: "global_decls"
    translatedcrateTraitDecls <- o .: "trait_decls"
    translatedcrateTraitImpls <- o .: "trait_impls"
    translatedcrateUnitMetadata <- o .: "unit_metadata"
    translatedcrateOrderedDecls <- o .: "ordered_decls"
    pure (TranslatedCrate translatedcrateCrateName translatedcrateOptions translatedcrateTargetInformation translatedcrateItemNames translatedcrateShortNames translatedcrateFiles translatedcrateTypeDecls translatedcrateFunDecls translatedcrateGlobalDecls translatedcrateTraitDecls translatedcrateTraitImpls translatedcrateUnitMetadata translatedcrateOrderedDecls)






