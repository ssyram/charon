{-# LANGUAGE OverloadedStrings #-}

module Test.Deserialize (tests) where

import Data.Aeson (eitherDecodeStrict, Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Directory (doesFileExist)
import Test.Tasty
import Test.Tasty.HUnit

-- Import generated modules (only what we need for basic validation)
import Generated_Meta
import Generated_Values
import Generated_Types

tests :: TestTree
tests = testGroup "Deserialization Tests"
  [ testCase "Parse: FileId from JSON string" test_fileId_parse
  , testCase "Parse: IntTy from JSON string" test_intTy_parse  
  , testCase "Parse: FloatType from JSON string" test_floatType_parse
  , testCase "Parse: UIntTy from JSON string" test_uIntTy_parse
  , testCase "Parse test LLBC file exists" test_llbc_exists
  ]

-- Test parsing basic types from JSON
test_fileId_parse :: Assertion
test_fileId_parse = do
  let json = BS8.pack "\"test/file.rs\""
  let result = eitherDecodeStrict json :: Either String FileId
  case result of
    Left err -> assertFailure $ "Failed to parse FileId: " ++ err
    Right fileId -> assertEqual "FileId value" "test/file.rs" fileId

test_intTy_parse :: Assertion
test_intTy_parse = do
  let json = BS8.pack "\"I32\""
  let result = eitherDecodeStrict json :: Either String IntTy
  case result of
    Left err -> assertFailure $ "Failed to parse IntTy: " ++ err
    Right intTy -> assertEqual "IntTy value" I32 intTy

test_floatType_parse :: Assertion
test_floatType_parse = do
  let json = BS8.pack "\"F64\""
  let result = eitherDecodeStrict json :: Either String FloatType
  case result of
    Left err -> assertFailure $ "Failed to parse FloatType: " ++ err
    Right floatType -> assertEqual "FloatType value" F64 floatType

test_uIntTy_parse :: Assertion
test_uIntTy_parse = do
  let json = BS8.pack "\"U8\""
  let result = eitherDecodeStrict json :: Either String UIntTy
  case result of
    Left err -> assertFailure $ "Failed to parse UIntTy: " ++ err
    Right uIntTy -> assertEqual "UIntTy value" U8 uIntTy

-- Test that the LLBC test file exists
test_llbc_exists :: Assertion
test_llbc_exists = do
  let testFile = "charon-hs/test/data/test_crate.llbc"
  fileExists <- doesFileExist testFile
  assertBool ("Test LLBC file should exist: " ++ testFile) fileExists

-- Note: Full LLBC parsing tests would require implementing all FromJSON instances
-- and the complete TranslatedCrate type. These basic tests validate the foundation.

