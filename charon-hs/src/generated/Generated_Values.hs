{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Values.hs`
by hand. Edit `templates/Values.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Values where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as H

data FloatType = F16
  | F32
  | F64
  | F128
  deriving (Show, Eq, Ord)

-- | This is simlar to the Scalar value above. However, instead of storing
-- | the float value itself, we store its String representation. This allows
-- | to derive the Eq and Ord traits, which are not implemented for floats
data FloatValue = FloatValue
  { floatValue :: String
  , floatTy :: FloatType
  }
  deriving (Show, Eq, Ord)

data IntTy = Isize
  | I8
  | I16
  | I32
  | I64
  | I128
  deriving (Show, Eq, Ord)

data IntegerType = Signed IntTy
  | Unsigned UIntTy
  deriving (Show, Eq, Ord)

-- | A primitive value.
-- | 
-- | Those are for instance used for the constant operands [crate::expressions::Operand::Const]
data Literal = VScalar ScalarValue
  | VFloat FloatValue
  | VBool Bool
  | VChar Char
  | VByteStr [Int]
  | VStr String
  deriving (Show, Eq, Ord)

-- | Types of primitive values. Either an integer, bool, char
data LiteralType = TInt IntTy
  | TuInt UIntTy
  | TFloat FloatType
  | TBool
  | TChar
  deriving (Show, Eq, Ord)

-- | A scalar value.
data ScalarValue = UnsignedScalar UIntTy Integer
  | SignedScalar IntTy Integer
  deriving (Show, Eq, Ord)

data UIntTy = Usize
  | U8
  | U16
  | U32
  | U64
  | U128
  deriving (Show, Eq, Ord)
