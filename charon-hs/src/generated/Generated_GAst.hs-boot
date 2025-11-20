module Generated_GAst where

import qualified Generated_Meta as M
import Data.Aeson (FromJSON)

newtype FunDeclId = FunDeclId { fundeclidRaw :: Int }
instance Show FunDeclId
instance Eq FunDeclId
instance Ord FunDeclId
instance FromJSON FunDeclId

newtype GlobalDeclId = GlobalDeclId { globaldeclidRaw :: Int }
instance Show GlobalDeclId
instance Eq GlobalDeclId
instance Ord GlobalDeclId
instance FromJSON GlobalDeclId

newtype TypeDeclId = TypeDeclId { typedeclidRaw :: Int }
instance Show TypeDeclId
instance Eq TypeDeclId
instance Ord TypeDeclId
instance FromJSON TypeDeclId

newtype TraitDeclId = TraitDeclId { traitdeclidRaw :: Int }
instance Show TraitDeclId
instance Eq TraitDeclId
instance Ord TraitDeclId
instance FromJSON TraitDeclId

newtype TraitImplId = TraitImplId { traitimplidRaw :: Int }
instance Show TraitImplId
instance Eq TraitImplId
instance Ord TraitImplId
instance FromJSON TraitImplId

data TraitItemName
instance Show TraitItemName
instance Eq TraitItemName
instance Ord TraitItemName
instance FromJSON TraitItemName
