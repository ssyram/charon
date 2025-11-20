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
data UIntTy
data FloatType
data Binder a
data DeBruijnVar a
data RegionId
data RegionParam
data IntTy
data TraitClauseId
data TraitDeclRef
data TraitRefKind
data TraitImplRef
data BuiltinImplData
data TypeDeclRef
data GenericParams
data ConstGenericVarId
data ConstGeneric
data DeBruijnId
data ConstGenericParam
data TypeParam
data TraitParam
data TraitTypeConstraint
data OutlivesPred a b
data TraitTypeConstraintId

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

instance Show UIntTy
instance Eq UIntTy
instance Ord UIntTy
instance FromJSON UIntTy

instance Show FloatType
instance Eq FloatType
instance Ord FloatType
instance FromJSON FloatType

instance (Show a) => Show (Binder a)
instance (Eq a) => Eq (Binder a)
instance (Ord a) => Ord (Binder a)
instance (FromJSON a) => FromJSON (Binder a)

instance (Show a) => Show (DeBruijnVar a)
instance (Eq a) => Eq (DeBruijnVar a)
instance (Ord a) => Ord (DeBruijnVar a)
instance (FromJSON a) => FromJSON (DeBruijnVar a)

instance Show RegionId
instance Eq RegionId
instance Ord RegionId
instance FromJSON RegionId

instance Show RegionParam
instance Eq RegionParam
instance Ord RegionParam
instance FromJSON RegionParam

instance Show IntTy
instance Eq IntTy
instance Ord IntTy
instance FromJSON IntTy

instance Show TraitClauseId
instance Eq TraitClauseId
instance Ord TraitClauseId
instance FromJSON TraitClauseId

instance Show TraitDeclRef
instance Eq TraitDeclRef
instance Ord TraitDeclRef
instance FromJSON TraitDeclRef

instance Show TraitRefKind
instance Eq TraitRefKind
instance Ord TraitRefKind
instance FromJSON TraitRefKind

instance Show TraitImplRef
instance Eq TraitImplRef
instance Ord TraitImplRef
instance FromJSON TraitImplRef

instance Show BuiltinImplData
instance Eq BuiltinImplData
instance Ord BuiltinImplData
instance FromJSON BuiltinImplData

instance Show TypeDeclRef
instance Eq TypeDeclRef
instance Ord TypeDeclRef
instance FromJSON TypeDeclRef

instance Show GenericParams
instance Eq GenericParams
instance Ord GenericParams
instance FromJSON GenericParams

instance Show ConstGenericVarId
instance Eq ConstGenericVarId
instance Ord ConstGenericVarId
instance FromJSON ConstGenericVarId

instance Show ConstGeneric
instance Eq ConstGeneric
instance Ord ConstGeneric
instance FromJSON ConstGeneric

instance Show DeBruijnId
instance Eq DeBruijnId
instance Ord DeBruijnId
instance FromJSON DeBruijnId

instance Show ConstGenericParam
instance Eq ConstGenericParam
instance Ord ConstGenericParam
instance FromJSON ConstGenericParam

instance Show TypeParam
instance Eq TypeParam
instance Ord TypeParam
instance FromJSON TypeParam

instance Show TraitParam
instance Eq TraitParam
instance Ord TraitParam
instance FromJSON TraitParam

instance Show TraitTypeConstraint
instance Eq TraitTypeConstraint
instance Ord TraitTypeConstraint
instance FromJSON TraitTypeConstraint

instance (Show a, Show b) => Show (OutlivesPred a b)
instance (Eq a, Eq b) => Eq (OutlivesPred a b)
instance (Ord a, Ord b) => Ord (OutlivesPred a b)
instance (FromJSON a, FromJSON b) => FromJSON (OutlivesPred a b)

instance Show TraitTypeConstraintId
instance Eq TraitTypeConstraintId
instance Ord TraitTypeConstraintId
instance FromJSON TraitTypeConstraintId
