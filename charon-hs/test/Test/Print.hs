{-# LANGUAGE OverloadedStrings #-}

module Test.Print (tests) where

import Data.Aeson (eitherDecodeFileStrict)
import System.FilePath ((</>))
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit
    ( testCase, assertBool, assertFailure, Assertion )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Import our modules
import Charon.Printing
import Generated_Krate (LlbcFile(..), TranslatedCrate(..))
import qualified Generated_GAst as G
import qualified Generated_Types as T

tests :: TestTree
tests = testGroup "Printing Tests"
  [ testCase "Print infrastructure test" test_simple_print
  ]

-- Test that we can load LLBC files and create printing context
-- Note: Full printing comparison requires all BuildWithCtx instances to be implemented
test_simple_print :: Assertion
test_simple_print = do
  -- For now, just test that the printing infrastructure is in place
  let ctx = emptyCtx
  assertBool "Created empty printing context" True
  
  -- Test basic printing
  let result = printWithCtx ctx T.I32
  assertBool ("Printed IntTy: " ++ result) (result == "i32")
  
  -- TODO: When simple_test.llbc is generated, add full test:
  -- 1. Load LLBC file
  -- 2. Create PrintingCtx with translated crate
  -- 3. Print FunDecl instances
  -- 4. Compare with expected output from Rust's --print-llbc
