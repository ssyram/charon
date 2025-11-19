{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Crate.hs`
by hand. Edit `templates/Crate.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Crate where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import Data.Text (Text, unpack)
import Generated_Meta
import Generated_Values
import Generated_Types hiding (Opaque)
import Generated_Expressions
import qualified Generated_GAst as G
import Generated_GAst hiding (Call, CopyNonOverlapping, Error)
import qualified Generated_LlbcAst as L
import qualified Generated_UllbcAst as U

-- Manually define Body with properly qualified types
-- The auto-generated version has ambiguous Block/BlockId references
data Body = Unstructured (GexprBody U.Blocks)
  | Structured (GexprBody L.Block)
  | TraitMethodWithoutDefault
  | Opaque
  | Missing
  | Error G.Error  -- Qualified to avoid ambiguity with variant constructor name
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

-- Manually define TranslatedCrate to fix HashMap type parameters
-- The auto-generated version includes RandomState which doesn't exist in Haskell's HashMap
-- Also, item_names and short_names are serialized as arrays (with HashMapToArray in Rust),
-- so we use [(ItemId, Name)] instead of HashMap ItemId Name
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
  translatedcrateItemNames :: [(ItemId, Name)]
  ,   -- | Short names, for items whose last PathElem is unique.
  translatedcrateShortNames :: [(ItemId, Name)]
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

instance FromJSON TranslatedCrate where
  parseJSON = withObject "TranslatedCrate" $ \o -> do
    translatedcrateCrateName <- o .: "crate_name"
    translatedcrateOptions <- o .: "options"
    translatedcrateTargetInformation <- o .: "target_information"
    -- Parse item_names and short_names from HashMapToArray format (array of {key, value} objects)
    itemNamesArray <- o .: "item_names"
    let translatedcrateItemNames = map (\obj -> case obj of
                                          Object o -> case (H.lookup "key" o, H.lookup "value" o) of
                                            (Just k, Just v) -> case (fromJSON k, fromJSON v) of
                                              (Success key, Success value) -> (key, value)
                                              _ -> error "Failed to parse item_names entry"
                                            _ -> error "Missing key or value in item_names entry"
                                          _ -> error "Expected object in item_names array"
                                      ) itemNamesArray
    shortNamesArray <- o .: "short_names"
    let translatedcrateShortNames = map (\obj -> case obj of
                                          Object o -> case (H.lookup "key" o, H.lookup "value" o) of
                                            (Just k, Just v) -> case (fromJSON k, fromJSON v) of
                                              (Success key, Success value) -> (key, value)
                                              _ -> error "Failed to parse short_names entry"
                                            _ -> error "Missing key or value in short_names entry"
                                          _ -> error "Expected object in short_names array"
                                        ) shortNamesArray
    translatedcrateFiles <- o .: "files"
    translatedcrateTypeDecls <- o .: "type_decls"
    translatedcrateFunDecls <- o .: "fun_decls"
    translatedcrateGlobalDecls <- o .: "global_decls"
    translatedcrateTraitDecls <- o .: "trait_decls"
    translatedcrateTraitImpls <- o .: "trait_impls"
    translatedcrateUnitMetadata <- o .: "unit_metadata"
    translatedcrateOrderedDecls <- o .: "ordered_decls"
    pure (TranslatedCrate translatedcrateCrateName translatedcrateOptions translatedcrateTargetInformation translatedcrateItemNames translatedcrateShortNames translatedcrateFiles translatedcrateTypeDecls translatedcrateFunDecls translatedcrateGlobalDecls translatedcrateTraitDecls translatedcrateTraitImpls translatedcrateUnitMetadata translatedcrateOrderedDecls)

{- __REPLACE0__ -}

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

{- __REPLACE1__ -}





