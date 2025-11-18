{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Types.hs`
by hand. Edit `templates/Types.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values

-- Manually defined type aliases and newtypes
-- TraitTypeConstraintId is a newtype wrapper around Int
newtype TraitTypeConstraintId = TraitTypeConstraintId { traittypeconstraintidRaw :: Int }
  deriving (Show, Eq, Ord)

{- __REPLACE0__ -}
