{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
WARNING: this file is partially auto-generated. Do not edit `Generated_GAst.hs`
by hand. Edit `templates/GAst.hs` instead, or improve the code
generation tool to avoid the need for hand-writing things.
-}

module Generated_GAst where

import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as H
import qualified Data.Vector as V
import qualified Generated_Meta as M
import qualified Generated_Types as T
import qualified Generated_Expressions as E
import {-# SOURCE #-} qualified Generated_LlbcAst as L
import {-# SOURCE #-} qualified Generated_UllbcAst as U
import qualified Generated_Krate as K

-- Re-export commonly used types for convenience
type Vector = M.Vector
type Span = M.Span
type PathBuf = M.PathBuf

-- Re-export Krate types for backward compatibility
type Body = K.Body
type FunDecl = K.FunDecl
type TranslatedCrate = K.TranslatedCrate

{- __REPLACE0__ -}

{- __REPLACE1__ -}

-- Wrapper type for the top-level LLBC file structure
-- This is a Haskell-specific convenience type, not from Rust
data LlbcFile = LlbcFile
  { llbcfileCharonVersion :: String
  , llbcfileTranslated :: TranslatedCrate
  }
  deriving (Show, Eq, Ord)

instance FromJSON LlbcFile where
  parseJSON = withObject "LlbcFile" $ \o -> do
    charonVersion <- o .: "charon_version"
    translated <- o .: "translated"
    pure $ LlbcFile charonVersion translated
