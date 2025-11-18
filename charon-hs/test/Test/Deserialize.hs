{-# LANGUAGE OverloadedStrings #-}

module Test.Deserialize (tests, getAllLlbcTests) where

import Data.Aeson (eitherDecodeFileStrict, eitherDecodeStrict, Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad (filterM)

-- Import generated modules
import Generated_Meta
import Generated_Values
import Generated_Types
import Generated_GAstOfJson ()  -- For the FromJSON instances

tests :: TestTree
tests = testGroup "Deserialization Tests"
  [ testGroup "Basic Type Parsing"
      [ testCase "Parse: FileId from JSON string" test_fileId_parse
      , testCase "Parse: IntTy from JSON string" test_intTy_parse  
      , testCase "Parse: FloatType from JSON string" test_floatType_parse
      , testCase "Parse: UIntTy from JSON string" test_uIntTy_parse
      ]
  , testGroup "LLBC File Deserialization" 
      [ testCase "Find test LLBC files" test_find_llbc_files
      -- Note: The comprehensive LLBC tests will be added dynamically below
      ]
  ]

-- Test parsing basic types from JSON
test_fileId_parse :: Assertion
test_fileId_parse = do
  let json = BS8.pack "\"test/file.rs\""
  let result = eitherDecodeStrict json :: Either String FileId
  case result of
    Left err -> assertFailure $ "Failed to parse FileId: " ++ err
    Right (FileId text) -> assertEqual "FileId value" "test/file.rs" text

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

-- Test that we can find LLBC files
test_find_llbc_files :: Assertion
test_find_llbc_files = do
  let testDir = "charon/tests/ui"
  dirExists <- doesDirectoryExist testDir
  assertBool ("Test directory should exist: " ++ testDir) dirExists
  
  when dirExists $ do
    llbcFiles <- findLlbcFiles testDir
    let fileCount = length llbcFiles
    assertBool ("Should find LLBC files in " ++ testDir ++ ", found " ++ show fileCount) (fileCount > 0)

-- Helper function to find all .llbc files in a directory recursively
findLlbcFiles :: FilePath -> IO [FilePath]
findLlbcFiles dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  files <- filterM doesFileExist fullPaths
  dirs <- filterM doesDirectoryExist fullPaths
  let llbcFiles = filter (\f -> takeExtension f == ".llbc") files
  subLlbcFiles <- concat <$> mapM findLlbcFiles dirs
  return (llbcFiles ++ subLlbcFiles)

-- Helper to import when statement
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

-- Function to create a test case for each LLBC file
-- This will be called from Main.hs to generate dynamic tests
createLlbcTest :: FilePath -> TestTree
createLlbcTest filepath = testCase filepath $ do
  -- Try to parse the LLBC file as JSON Value first
  result <- eitherDecodeFileStrict filepath :: IO (Either String Value)
  case result of
    Left err -> assertFailure $ "Failed to parse " ++ filepath ++ ": " ++ err
    Right _ -> return () -- Success - parsed as valid JSON

-- Export function to get all LLBC test cases
getAllLlbcTests :: IO TestTree
getAllLlbcTests = do
  let testDir = "charon/tests/ui"
  dirExists <- doesDirectoryExist testDir
  if not dirExists
    then return $ testGroup "LLBC Files" []
    else do
      llbcFiles <- findLlbcFiles testDir
      let testCases = map createLlbcTest llbcFiles
      return $ testGroup "All LLBC Files" testCases


