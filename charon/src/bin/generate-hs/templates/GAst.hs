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

-- Re-export commonly used types for convenience
type Vector = M.Vector
type Span = M.Span
type PathBuf = M.PathBuf

{- __REPLACE0__ -}

{- __REPLACE1__ -}
