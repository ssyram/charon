{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Meta.hs`
by hand. Edit `templates/Meta.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.

`templates/Meta.hs` contains the manual definitions and some `{- __REPLACEn__ -}`
comments. These comments are replaced by auto-generated definitions by running
`make generate-hs` in the crate root. The code-generation code is in
`charon/src/bin/generate-hs`.
-}

module Generated_Meta where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (catMaybes)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import {-# SOURCE #-} qualified Generated_Types as T
import {-# SOURCE #-} qualified Generated_GAst as G
import {-# SOURCE #-} qualified Generated_Expressions as E
import {-# SOURCE #-} qualified Generated_Values as Val

-- Using newtype instead of type alias to avoid duplicate instance issues
newtype PathBuf = PathBuf Text
  deriving (Show, Eq, Ord)

instance FromJSON PathBuf where
  parseJSON v = PathBuf <$> parseJSON v

-- Vector is used for indexed sequences in Rust (IndexVec in charon)
-- Defined here to avoid circular dependencies
newtype Vector k v = Vector [v]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance FromJSON b => FromJSON (Vector a b) where
  parseJSON = fmap (Vector . catMaybes) . parseJSON

-- KVPair is used to deserialize HashMap serialized with HashMapToArray
-- which creates array of {key, value} objects instead of tuples
data KVPair k v = KVPair { kvpairKey :: k, kvpairValue :: v }
  deriving (Show, Eq, Ord)

instance (FromJSON k, FromJSON v) => FromJSON (KVPair k v) where
  parseJSON = withObject "KVPair" $ \o -> do
    key <- o .: "key"
    value <- o .: "value"
    pure (KVPair key value)

-- Helper function to parse Integer values that are serialized as strings
parseIntegerValue :: Value -> Parser Integer
parseIntegerValue v = case v of
  String s -> case reads (Text.unpack s) of
    [(n, "")] -> pure n
    _ -> fail $ "Failed to parse integer from string: " ++ Text.unpack s
  Number n -> pure (floor n)
  _ -> fail "Expected String or Number for integer value"

{- __REPLACE0__ -}

{- __REPLACE1__ -}
