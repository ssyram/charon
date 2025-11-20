module Generated_GAst where

import Data.Aeson (FromJSON)

-- Type declarations needed by Types module
data TraitDeclId
data TraitItemName
data GlobalDeclId
data FunDeclId
data TypeDeclId
data TraitImplId
data ItemSource
data ItemMeta
data TraitAssocTyImpl

instance Show TraitDeclId
instance Eq TraitDeclId
instance Ord TraitDeclId
instance FromJSON TraitDeclId

instance Show TraitItemName
instance Eq TraitItemName
instance Ord TraitItemName
instance FromJSON TraitItemName

instance Show GlobalDeclId
instance Eq GlobalDeclId
instance Ord GlobalDeclId
instance FromJSON GlobalDeclId

instance Show FunDeclId
instance Eq FunDeclId
instance Ord FunDeclId
instance FromJSON FunDeclId

instance Show TypeDeclId
instance Eq TypeDeclId
instance Ord TypeDeclId
instance FromJSON TypeDeclId

instance Show TraitImplId
instance Eq TraitImplId
instance Ord TraitImplId
instance FromJSON TraitImplId

instance Show ItemSource
instance Eq ItemSource
instance Ord ItemSource
instance FromJSON ItemSource

instance Show ItemMeta
instance Eq ItemMeta
instance Ord ItemMeta
instance FromJSON ItemMeta

instance Show TraitAssocTyImpl
instance Eq TraitAssocTyImpl
instance Ord TraitAssocTyImpl
instance FromJSON TraitAssocTyImpl
