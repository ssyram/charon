{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_Values.hs`
by hand. Edit `templates/Values.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_Values where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
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

{- __REPLACE0__ -}

{- __REPLACE1__ -}
