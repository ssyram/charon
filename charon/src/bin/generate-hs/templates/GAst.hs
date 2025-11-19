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

import Data.Aeson
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

{- __REPLACE0__ -}

{- __REPLACE1__ -}

