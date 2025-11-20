module Generated_Values where

import Data.Aeson (FromJSON)

data Literal
data ScalarValue
data FloatValue

instance Show Literal
instance Eq Literal
instance Ord Literal
instance FromJSON Literal

instance Show ScalarValue
instance Eq ScalarValue
instance Ord ScalarValue
instance FromJSON ScalarValue

instance Show FloatValue
instance Eq FloatValue
instance Ord FloatValue
instance FromJSON FloatValue
