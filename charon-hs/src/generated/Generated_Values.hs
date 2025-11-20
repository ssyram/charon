{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Values.hs`
by hand. Edit `templates/Values.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Values where

import Data.Aeson (FromJSON, Value(..), parseJSON)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V

-- Helper to parse Integer from either String or Number
parseIntegerValue :: Value -> Parser Integer
parseIntegerValue (String s) = case reads (T.unpack s) of
  [(n, "")] -> return n
  _ -> fail $ "Failed to parse Integer from string: " ++ T.unpack s
parseIntegerValue (Number n) = parseJSON (Number n)
parseIntegerValue v = fail $ "Expected String or Number for Integer, got: " ++ show v

data FloatType = F16
  | F32
  | F64
  | F128
  deriving (Show, Eq, Ord)

-- | This is simlar to the Scalar value above. However, instead of storing
-- | the float value itself, we store its String representation. This allows
-- | to derive the Eq and Ord traits, which are not implemented for floats
data FloatValue = FloatValue
  { floatvalueFloatValue :: String
  , floatvalueFloatTy :: T.FloatType
  }
  deriving (Show, Eq, Ord)

data IntTy = Isize
  | I8
  | I16
  | I32
  | I64
  | I128
  deriving (Show, Eq, Ord)

data IntegerType = Signed T.IntTy
  | Unsigned T.UIntTy
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
data LiteralType = TInt T.IntTy
  | TuInt T.UIntTy
  | TFloat T.FloatType
  | TBool
  | TChar
  deriving (Show, Eq, Ord)

-- | A scalar value.
data ScalarValue = UnsignedScalar T.UIntTy Integer
  | SignedScalar T.IntTy Integer
  deriving (Show, Eq, Ord)

data UIntTy = Usize
  | U8
  | U16
  | U32
  | U64
  | U128
  deriving (Show, Eq, Ord)

instance FromJSON FloatType where
  parseJSON v = case v of
    String "F16" -> pure F16
    String "F32" -> pure F32
    String "F64" -> pure F64
    String "F128" -> pure F128
    _ -> fail "Unknown variant"


instance FromJSON FloatValue where
  parseJSON = withObject "FloatValue" $ \o -> do
    floatvalueFloatValue <- o .: "value"
    floatvalueFloatTy <- o .: "ty"
    pure (FloatValue floatvalueFloatValue floatvalueFloatTy)


instance FromJSON IntTy where
  parseJSON v = case v of
    String "Isize" -> pure Isize
    String "I8" -> pure I8
    String "I16" -> pure I16
    String "I32" -> pure I32
    String "I64" -> pure I64
    String "I128" -> pure I128
    _ -> fail "Unknown variant"


instance FromJSON IntegerType where
  parseJSON v = case v of
    Object o | H.lookup "Signed" o /= Nothing -> do
      v <- o .: "Signed"
      Signed <$> parseJSON v
    Object o | H.lookup "Unsigned" o /= Nothing -> do
      v <- o .: "Unsigned"
      Unsigned <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON Literal where
  parseJSON v = case v of
    Object o | H.lookup "Scalar" o /= Nothing -> do
      v <- o .: "Scalar"
      VScalar <$> parseJSON v
    Object o | H.lookup "Float" o /= Nothing -> do
      v <- o .: "Float"
      VFloat <$> parseJSON v
    Object o | H.lookup "Bool" o /= Nothing -> do
      v <- o .: "Bool"
      VBool <$> parseJSON v
    Object o | H.lookup "Char" o /= Nothing -> do
      v <- o .: "Char"
      VChar <$> parseJSON v
    Object o | H.lookup "ByteStr" o /= Nothing -> do
      v <- o .: "ByteStr"
      VByteStr <$> parseJSON v
    Object o | H.lookup "Str" o /= Nothing -> do
      v <- o .: "Str"
      VStr <$> parseJSON v
    _ -> fail "Unknown variant"


instance FromJSON LiteralType where
  parseJSON v = case v of
    Object o | H.lookup "Int" o /= Nothing -> do
      v <- o .: "Int"
      TInt <$> parseJSON v
    Object o | H.lookup "UInt" o /= Nothing -> do
      v <- o .: "UInt"
      TuInt <$> parseJSON v
    Object o | H.lookup "Float" o /= Nothing -> do
      v <- o .: "Float"
      TFloat <$> parseJSON v
    String "Bool" -> pure TBool
    String "Char" -> pure TChar
    _ -> fail "Unknown variant"


instance FromJSON ScalarValue where
  parseJSON v = case v of
    Object o | H.lookup "Unsigned" o /= Nothing -> do
      withArray "UnsignedScalar" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseIntegerValue (v V.! 1)
        pure (UnsignedScalar v0 v1)) =<< o .: "Unsigned"
    Object o | H.lookup "Signed" o /= Nothing -> do
      withArray "SignedScalar" (\v -> do
        v0 <- parseJSON (v V.! 0)
        v1 <- parseIntegerValue (v V.! 1)
        pure (SignedScalar v0 v1)) =<< o .: "Signed"
    _ -> fail "Unknown variant"


instance FromJSON UIntTy where
  parseJSON v = case v of
    String "Usize" -> pure Usize
    String "U8" -> pure U8
    String "U16" -> pure U16
    String "U32" -> pure U32
    String "U64" -> pure U64
    String "U128" -> pure U128
    _ -> fail "Unknown variant"

