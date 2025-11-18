{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_GAst.hs`
by hand. Edit `templates/GAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_GAst where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_Expressions

-- Manually defined types

-- Vector is used for indexed sequences in Rust (IndexVec in charon)
-- The key type is just for type safety, the actual storage is just a list
-- where the index is implicit from the position
type Vector k v = [v]

data TargetInfo = TargetInfo
  { targetinfoTargetPointerSize :: Int
  , targetinfoIsLittleEndian :: Bool
  }
  deriving (Show, Eq, Ord)

-- Types with name conflicts - defined manually to avoid issues with Types module
data Local = Local
  { localIndex :: LocalId
  , localName :: (Maybe String)
  , localLocalTy :: Ty
  }
  deriving (Show, Eq, Ord)

data TraitImpl = TraitImpl
  { traitimplDefId :: TraitImplId
  , traitimplItemMeta :: ItemMeta
  , traitimplImplTrait :: TraitDeclRef
  , traitimplGenerics :: GenericParams
  , traitimplImpliedTraitRefs :: Vector TraitClauseId TraitRef
  , traitimplConsts :: [(TraitItemName, GlobalDeclRef)]
  , traitimplTypes :: [(TraitItemName, (Binder TraitAssocTyImpl))]
  , traitimplMethods :: [(TraitItemName, (Binder FunDeclRef))]
  , traitimplVtable :: (Maybe GlobalDeclRef)
  }
  deriving (Show, Eq, Ord)

data TraitMethod = TraitMethod
  { traitmethodName :: TraitItemName
  , traitmethodItem :: FunDeclRef
  }
  deriving (Show, Eq, Ord)

data Assertion = Assertion
  { assertionCond :: Operand
  , assertionExpected :: Bool
  }
  deriving (Show, Eq, Ord)

data Call = Call
  { callFunc :: FnOperand
  , callGenerics :: GenericArgs
  , callArgs :: [Operand]
  , callDest :: Place
  }
  deriving (Show, Eq, Ord)

data CopyNonOverlapping = CopyNonOverlapping
  { copysrc :: Operand
  , copydst :: Operand
  , copycount :: Operand
  }
  deriving (Show, Eq, Ord)

{- __REPLACE0__ -}
