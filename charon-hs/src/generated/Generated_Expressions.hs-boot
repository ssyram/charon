module Generated_Expressions where

import Data.Aeson (FromJSON)

-- Type declarations needed by Types module
data BuiltinIndexOp
data FnPtrKind
data FunId
data BuiltinFunId
data FnPtr

instance Show BuiltinIndexOp
instance Eq BuiltinIndexOp
instance Ord BuiltinIndexOp
instance FromJSON BuiltinIndexOp

instance Show FnPtrKind
instance Eq FnPtrKind
instance Ord FnPtrKind
instance FromJSON FnPtrKind

instance Show FunId
instance Eq FunId
instance Ord FunId
instance FromJSON FunId

instance Show BuiltinFunId
instance Eq BuiltinFunId
instance Ord BuiltinFunId
instance FromJSON BuiltinFunId

instance Show FnPtr
instance Eq FnPtr
instance Ord FnPtr
instance FromJSON FnPtr
