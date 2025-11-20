module Generated_Types where

import Data.Aeson (FromJSON)

-- Type declarations needed by Meta and other modules
data BuiltinTy
data TypeVarId
data Ty
data TypeId
data RegionBinder a
data LiteralType
data Region
data RefKind
data TraitRef
data DynPredicate
data GenericArgs

instance Show BuiltinTy
instance Eq BuiltinTy
instance Ord BuiltinTy
instance FromJSON BuiltinTy

instance Show TypeVarId
instance Eq TypeVarId
instance Ord TypeVarId
instance FromJSON TypeVarId

instance Show Ty
instance Eq Ty
instance Ord Ty
instance FromJSON Ty

instance Show TypeId
instance Eq TypeId
instance Ord TypeId
instance FromJSON TypeId

instance (Show a) => Show (RegionBinder a)
instance (Eq a) => Eq (RegionBinder a)
instance (Ord a) => Ord (RegionBinder a)
instance (FromJSON a) => FromJSON (RegionBinder a)

instance Show LiteralType
instance Eq LiteralType
instance Ord LiteralType
instance FromJSON LiteralType

instance Show Region
instance Eq Region
instance Ord Region
instance FromJSON Region

instance Show RefKind
instance Eq RefKind
instance Ord RefKind
instance FromJSON RefKind

instance Show TraitRef
instance Eq TraitRef
instance Ord TraitRef
instance FromJSON TraitRef

instance Show DynPredicate
instance Eq DynPredicate
instance Ord DynPredicate
instance FromJSON DynPredicate

instance Show GenericArgs
instance Eq GenericArgs
instance Ord GenericArgs
instance FromJSON GenericArgs
