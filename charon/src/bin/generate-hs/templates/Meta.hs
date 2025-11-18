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
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as H

-- Using newtype instead of type alias to avoid duplicate instance issues
newtype PathBuf = PathBuf Text
  deriving (Show, Eq, Ord)

instance FromJSON PathBuf where
  parseJSON v = PathBuf <$> parseJSON v

-- Vector is used for indexed sequences in Rust (IndexVec in charon)
-- Defined here to avoid circular dependencies
newtype Vector k v = Vector [v]
  deriving (Show, Eq, Ord)

instance FromJSON b => FromJSON (Vector a b) where
  parseJSON = fmap (Vector . catMaybes) . parseJSON

{- __REPLACE0__ -}
