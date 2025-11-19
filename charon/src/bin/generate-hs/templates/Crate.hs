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





