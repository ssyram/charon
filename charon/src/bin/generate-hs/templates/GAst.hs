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
import qualified Data.Aeson.KeyMap as H
import Generated_Meta hiding (Local)
import Generated_Types hiding (Field, TraitImpl, TraitMethod)
import Generated_Expressions hiding (Field)

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

-- Common error used during the translation.
-- This is defined here instead of in Crate module to avoid name collision
-- with the Body::Error variant constructor
data Error = Error
  { errorSpan :: Span
  , errorMsg :: String
  }
  deriving (Show, Eq, Ord)

instance FromJSON Error where
  parseJSON = withObject "Error" $ \o -> do
    errorSpan <- o .: "span"
    errorMsg <- o .: "msg"
    pure (Error errorSpan errorMsg)

{- __REPLACE0__ -}

{- __REPLACE1__ -}

