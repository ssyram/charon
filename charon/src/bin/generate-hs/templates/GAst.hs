{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_GAst.hs`
by hand. Edit `templates/GAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_GAst where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import Generated_Meta hiding (Local, Error)
import qualified Generated_Meta as M
import Generated_Types hiding (Field, TraitImpl, TraitMethod, Opaque)
import Generated_Expressions hiding (Field)
import {-# SOURCE #-} qualified Generated_LlbcAst as L
import {-# SOURCE #-} qualified Generated_UllbcAst as U

-- Manually defined types

data TargetInfo = TargetInfo
  { targetinfoTargetPointerSize :: Int
  , targetinfoIsLittleEndian :: Bool
  }
  deriving (Show, Eq, Ord)

instance FromJSON TargetInfo where
  parseJSON = withObject "TargetInfo" $ \o -> do
    targetPointerSize <- o .: "target_pointer_size"
    isLittleEndian <- o .: "is_little_endian"
    pure (TargetInfo targetPointerSize isLittleEndian)

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
