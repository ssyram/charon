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
import Generated_Values
import Generated_Types hiding (TraitImpl, TraitMethod)
import qualified Generated_Types as T
import Generated_Expressions hiding (Field)
import Generated_GAst

-- Vector is manually defined here since it's excluded from generation
type Vector a b = [(a, b)]

{- __REPLACE0__ -}
