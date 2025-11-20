module Generated_Types where

import Data.Aeson (FromJSON)

-- Type declarations needed by Meta module
data BuiltinTy
data TypeVarId

instance Show BuiltinTy
instance Eq BuiltinTy
instance Ord BuiltinTy
instance FromJSON BuiltinTy

instance Show TypeVarId
instance Eq TypeVarId
instance Ord TypeVarId
instance FromJSON TypeVarId
