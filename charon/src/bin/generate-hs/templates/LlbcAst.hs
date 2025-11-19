{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_LlbcAst.hs`
by hand. Edit `templates/LlbcAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_LlbcAst where

import Data.Aeson hiding (Error)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions
import qualified Generated_GAst as G
-- Import everything from GAst except the data constructors that conflict with our variant constructors
import Generated_GAst hiding (Call, CopyNonOverlapping)

{- __REPLACE0__ -}

{- __REPLACE1__ -}
