{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_GAst.hs`
by hand. Edit `templates/GAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_GAst where

import Data.Aeson
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import qualified Generated_Meta as M
import Generated_Meta hiding (Local)
import Generated_Values
import qualified Generated_Types as T
import Generated_Types hiding (Field, TraitImpl, TraitMethod)
import qualified Generated_Expressions as E
import Generated_Expressions hiding (Field)

-- Manually defined types

data TargetInfo = TargetInfo
  { targetinfoTargetPointerSize :: Int
  , targetinfoIsLittleEndian :: Bool
  }
  deriving (Show, Eq, Ord)

{- __REPLACE0__ -}

-- Manual instance for TranslatedCrate - simplified version that parses the key fields
-- Full deserialization would require FunDecl and Body instances which have complex dependencies
data TranslatedCrate = TranslatedCrate
  { translatedCrateCrate_name :: String
  , translatedCrateType_decls :: Vector TypeDeclId TypeDecl
  , translatedCrateGlobal_decls :: Vector GlobalDeclId GlobalDecl
  , translatedCrateTrait_decls :: Vector TraitDeclId TraitDecl
  , translatedCrateTrait_impls :: Vector TraitImplId TraitImpl
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

{- __REPLACE1__ -}
