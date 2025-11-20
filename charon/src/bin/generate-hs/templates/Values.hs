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
import qualified Data.Text as Text
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
-- Note: Types is generated later but Values depends on it, so we need this import
-- This is handled by the extraction order in generate-hs
import qualified Generated_Types as T

-- Helper to parse Integer from either String or Number
parseIntegerValue :: Value -> Parser Integer
parseIntegerValue (String s) = case reads (Text.unpack s) of
  [(n, "")] -> return n
  _ -> fail $ "Failed to parse Integer from string: " ++ Text.unpack s
parseIntegerValue (Number n) = parseJSON (Number n)
parseIntegerValue v = fail $ "Expected String or Number for Integer, got: " ++ show v

{- __REPLACE0__ -}

{- __REPLACE1__ -}
