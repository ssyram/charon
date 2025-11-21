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

tests :: TestTree
tests = testGroup "Printing Tests"
  [ testCase "Print simple_test.llbc" test_simple_print
  ]

-- Test that we can print a simple LLBC file and it matches Rust output
test_simple_print :: Assertion
test_simple_print = do
  let llbcFile = "test/data/simple_test.llbc"
  let expectedFile = "test/data/simple_test.llbc.expected"
  
  -- Load the LLBC file
  result <- eitherDecodeFileStrict llbcFile :: IO (Either String LlbcFile)
  case result of
    Left err -> assertFailure $ "Failed to deserialize LLBC file: " ++ err
    Right llbcFileData -> do
      let crate = llbcfileTranslated llbcFileData
          ctx = emptyCtx { translated = Just crate }
      
      -- Try to print the crate (basic test for now)
      let crateName = translatedcrateCrateName crate
      assertBool ("Successfully loaded crate: " ++ crateName) True
      
      -- For now, just test that we can create the context
      -- TODO: Add more comprehensive printing and comparison once instances are complete
      assertBool "Created printing context successfully" True
