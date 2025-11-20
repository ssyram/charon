{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Generated_Krate where

import Data.Aeson (FromJSON(..), Value(..), withObject, withArray, (.:), (.!=))
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import qualified Generated_Meta as M
import qualified Generated_Types as T
import qualified Generated_Values as Val
import qualified Generated_Expressions as E
import qualified Generated_GAst as G
import qualified Generated_LlbcAst as L
import qualified Generated_UllbcAst as U

{- __REPLACE0__ -}

{- __REPLACE1__ -}

-- Wrapper type for the top-level LLBC file structure
-- This is a Haskell-specific convenience type, not from Rust
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
