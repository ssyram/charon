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

{- __REPLACE0__ -}
