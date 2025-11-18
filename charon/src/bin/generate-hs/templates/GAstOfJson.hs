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
import Generated_Meta hiding (Local)
import qualified Generated_Meta as M
import Generated_Values
import qualified Generated_Types as T
import Generated_Types hiding (TraitImpl, TraitMethod, Field, Local)
import Generated_Expressions
import Generated_GAst (Preset(..), TargetInfo(..), TraitAssocConst(..), TraitAssocTy(..), TraitDecl(..), TraitImpl(..), MirLevel(..), MonomorphizeMut(..), GlobalKind(..), Locals(..), GDeclarationGroup(..), GexprBody(..), GlobalDecl(..), CliOptions(..), DeclarationGroup(..), FnOperand(..), FunSig(..))
import qualified Generated_GAst as G

-- Vector is manually defined here since it's excluded from generation
type Vector a b = [(a, b)]

-- Manual instances for types that have name conflicts between GAst structs and Types variants/fields
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
    pure $ G.TraitImpl traitimplDefId traitimplItemMeta traitimplImplTrait traitimplGenerics traitimplImpliedTraitRefs traitimplConsts traitimplTypes traitimplMethods traitimplVtable

instance FromJSON G.TraitMethod where
  parseJSON = withObject "TraitMethod" $ \o -> do
    traitmethodName <- o .: "name"
    traitmethodItem <- o .: "item"
    pure $ G.TraitMethod traitmethodName traitmethodItem

instance FromJSON G.Field where
  parseJSON = withObject "Field" $ \o -> do
    fieldSpan <- o .: "span"
    fieldAttrInfo <- o .: "attr_info"
    fieldFieldName <- o .: "name"
    fieldFieldTy <- o .: "ty"
    pure $ G.Field fieldSpan fieldAttrInfo fieldFieldName fieldFieldTy

instance FromJSON G.Local where
  parseJSON = withObject "Local" $ \o -> do
    localIndex <- o .: "index"
    localName <- o .: "name"
    localLocalTy <- o .: "ty"
    pure $ G.Local localIndex localName localLocalTy

instance FromJSON G.Assertion where
  parseJSON = withObject "Assertion" $ \o -> do
    assertionCond <- o .: "cond"
    assertionExpected <- o .: "expected"
    pure $ G.Assertion assertionCond assertionExpected

instance FromJSON G.Call where
  parseJSON = withObject "Call" $ \o -> do
    callFunc <- o .: "func"
    callGenerics <- o .: "generics"
    callArgs <- o .: "args"
    callDest <- o .: "dest"
    pure $ G.Call callFunc callGenerics callArgs callDest

instance FromJSON G.CopyNonOverlapping where
  parseJSON = withObject "CopyNonOverlapping" $ \o -> do
    copysrc <- o .: "src"
    copydst <- o .: "dst"
    copycount <- o .: "count"
    pure $ G.CopyNonOverlapping copysrc copydst copycount

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

instance FromJSON TranslatedCrate where
  parseJSON = withObject "TranslatedCrate" $ \o -> do
    crateName <- o .: "crate_name"
    typeDecls <- o .: "type_decls"
    globalDecls <- o .: "global_decls"
    traitDecls <- o .: "trait_decls"
    traitImpls <- o .: "trait_impls"
    -- We skip fields that we can't deserialize yet (options, target_information, fun_decls, etc.)
    pure $ TranslatedCrate crateName typeDecls globalDecls traitDecls traitImpls

{- __REPLACE0__ -}
