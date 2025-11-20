{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Types.hs`
by hand. Edit `templates/Types.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Types where

import Data.Aeson (FromJSON(..), Value(..), withObject, withArray, (.:), (.!=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import qualified Generated_Meta as M
import qualified Generated_Values as Val

-- Re-export commonly used types for convenience
type Vector = M.Vector

-- Re-export parseIntegerValue helper from Meta
parseIntegerValue :: Value -> Parser Integer
parseIntegerValue = M.parseIntegerValue

-- Manually defined type aliases and newtypes
-- TraitTypeConstraintId is a newtype wrapper around Int
newtype TraitTypeConstraintId = TraitTypeConstraintId { traittypeconstraintidRaw :: Int }
  deriving (Show, Eq, Ord)

instance FromJSON TraitTypeConstraintId where
  parseJSON v = TraitTypeConstraintId <$> parseJSON v

{- __REPLACE0__ -}

{- __REPLACE1__ -}
