{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Types.hs`
by hand. Edit `templates/Types.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import Generated_Meta
import Generated_Values

-- Manually defined type aliases and newtypes
-- TraitTypeConstraintId is a newtype wrapper around Int
newtype TraitTypeConstraintId = TraitTypeConstraintId { traittypeconstraintidRaw :: Int }
  deriving (Show, Eq, Ord)

{- __REPLACE0__ -}

{- __REPLACE1__ -}
