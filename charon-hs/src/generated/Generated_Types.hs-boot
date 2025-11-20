module Generated_Types where

import Data.Aeson (FromJSON)

newtype TraitTypeConstraintId = TraitTypeConstraintId { traittypeconstraintidRaw :: Int }

instance Show TraitTypeConstraintId
instance Eq TraitTypeConstraintId
instance Ord TraitTypeConstraintId
instance FromJSON TraitTypeConstraintId

data Ty

instance Show Ty
instance Eq Ty
instance Ord Ty
instance FromJSON Ty
