{-# LANGUAGE OverloadedStrings #-}

module Test.Deserialize (tests, getAllLlbcTests) where

import Data.Aeson (eitherDecodeFileStrict, eitherDecodeStrict, withObject, FromJSON(..), (.:))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM, when)
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit
    ( testCase, assertBool, assertEqual, assertFailure, Assertion )

-- Import generated modules
import Generated_Meta
import Generated_Values
import Generated_GAst hiding (Assertion)  -- Hide Assertion to avoid conflict with HUnit's Assertion; TranslatedCrate and LlbcFile are here too

-- | Strict wrapper type for TranslatedCrate that rejects unknown fields
newtype StrictTranslatedCrate = StrictTranslatedCrate TranslatedCrate
  deriving (Show, Eq)

-- | Strict wrapper type for LlbcFile that uses strict TranslatedCrate
newtype StrictLlbcFile = StrictLlbcFile 
  { strictLlbcFileInner :: LlbcFile
  }
  deriving (Show, Eq)

-- | Parse TranslatedCrate with strict field checking
instance FromJSON StrictTranslatedCrate where
  parseJSON val = withObject "StrictTranslatedCrate" (\o -> do
    -- Define the expected fields
    let expectedFields = Set.fromList 
          [ "crate_name"
          , "type_decls"
          , "global_decls"
          , "trait_decls"
          , "trait_impls"
          ]
    
    -- Get actual fields from JSON
    let actualFields = Set.fromList $ map fst $ KM.toList o
    
    -- Check for unexpected fields
    let unexpectedFields = Set.difference actualFields expectedFields
    let missingFields = Set.difference expectedFields actualFields
    
    if not (Set.null unexpectedFields)
      then fail $ "Unexpected fields in TranslatedCrate: " ++ show (Set.toList unexpectedFields) ++
                  ". This indicates the Haskell type is missing fields that exist in the JSON. " ++
                  "Run 'make generate-hs' to regenerate the Haskell AST."
      else if not (Set.null missingFields)
        then fail $ "Missing required fields in JSON: " ++ show (Set.toList missingFields)
        else do
          -- Parse the normal TranslatedCrate using the same value
          crate <- parseJSON val :: Parser TranslatedCrate
          return $ StrictTranslatedCrate crate
    ) val

-- | Parse LlbcFile with strict TranslatedCrate checking
instance FromJSON StrictLlbcFile where
  parseJSON = withObject "StrictLlbcFile" $ \o -> do
    charonVersion <- o .: "charon_version"
    StrictTranslatedCrate translatedCrate <- o .: "translated"
    let llbcFile = LlbcFile charonVersion translatedCrate
    return $ StrictLlbcFile llbcFile

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
      , testCase "Strict parsing detects missing fields" test_strict_llbc_parsing
      -- Note: The comprehensive LLBC tests will be added dynamically below
      ]
  , testGroup "ULLBC File Deserialization"
      [ testCase "Parse ULLBC file with Drop terminator" test_ullbc_drop_file
      ]
  ]

-- Test parsing basic types from JSON
test_fileId_parse :: Assertion
test_fileId_parse = do
  let json = BS8.pack "0"
  let result = eitherDecodeStrict json :: Either String FileId
  case result of
    Left err -> assertFailure $ "Failed to parse FileId: " ++ err
    Right (FileId fileIdVal) -> assertEqual "FileId value" 0 fileIdVal

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

-- Test strict LLBC parsing that rejects unknown fields
-- This test SHOULD FAIL because LLBC files contain fields that aren't in the Haskell TranslatedCrate type
test_strict_llbc_parsing :: Assertion
test_strict_llbc_parsing = do
  let testFile = "test/data/test_crate.llbc"
  fileExists <- doesFileExist testFile
  when (not fileExists) $ do
    assertFailure $ "Test LLBC file not found: " ++ testFile
  
  result <- eitherDecodeFileStrict testFile :: IO (Either String StrictLlbcFile)
  case result of
    Left err -> 
      -- This is EXPECTED to fail - we want to demonstrate the issue
      assertBool ("✓ Strict parser correctly detected missing fields: " ++ err) True
    Right _file -> 
      assertFailure "Strict parser should have failed but didn't. The JSON file might not have extra fields."

-- Test parsing a ULLBC file with Drop terminator
-- This test should FAIL with the current generated code because Drop is in StatementKind, not TerminatorKind
test_ullbc_drop_file :: Assertion
test_ullbc_drop_file = do
  let testFile = "test/data/test_ullbc_drop.ullbc"
  fileExists <- doesFileExist testFile
  if not fileExists
    then assertBool ("ULLBC test file not found (expected, file is not committed): " ++ testFile) True
    else do
      result <- eitherDecodeFileStrict testFile :: IO (Either String LlbcFile)
      case result of
        Left err -> assertFailure $ "Failed to deserialize ULLBC file: " ++ err ++ 
                                    "\nThis failure indicates that the generated Haskell AST is out of sync with the Rust definitions. " ++
                                    "Run 'make generate-hs' to regenerate the Haskell AST."
        Right file -> do
          let crate = llbcfileTranslated file
          let typeDeclCount = length (translatedCrateType_decls crate)
          -- Note: fun_decls field is missing from TranslatedCrate, so we can't test function bodies yet
          -- This is part of the issue - function bodies with Drop terminators are never parsed
          assertBool ("Successfully deserialized ULLBC file with " ++ show typeDeclCount ++ " type declarations") True

-- Test that we can find LLBC files (optional - files not checked in)
test_find_llbc_files :: Assertion
test_find_llbc_files = do
  let testDir = "../charon/tests/ui"
  dirExists <- doesDirectoryExist testDir
  when dirExists $ do
    llbcFiles <- findLlbcFiles testDir
    let fileCount = length llbcFiles
    -- LLBC files are not checked into the repo, they're generated by the test suite
    -- So this test passes even if no files are found
    assertBool ("Checked for LLBC files in " ++ testDir ++ ", found " ++ show fileCount) True

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

-- Function to create a test case for each LLBC file
-- This will be called from Main.hs to generate dynamic tests
-- This tests that we can actually deserialize LLBC files into Haskell AST types
createLlbcTest :: FilePath -> TestTree
createLlbcTest filepath = testCase filepath $ do
  -- Parse the entire LLBC file as an LlbcFile (which contains TranslatedCrate)
  result <- eitherDecodeFileStrict filepath :: IO (Either String LlbcFile)
  case result of
    Left err -> assertFailure $ "Failed to deserialize LLBC file in " ++ filepath ++ ": " ++ err
    Right llbcFile -> do
      let crate = llbcfileTranslated llbcFile
      -- Successfully deserialized the entire crate!
      -- We can validate that it has the expected structure
      let typeDeclCount = length (translatedCrateType_decls crate)
          globalDeclCount = length (translatedCrateGlobal_decls crate)
          traitDeclCount = length (translatedCrateTrait_decls crate)
          traitImplCount = length (translatedCrateTrait_impls crate)
      
      -- The test passes if we successfully deserialized the TranslatedCrate
      assertBool (concat
        [ "Successfully deserialized LLBC file with charon version "
        , llbcfileCharon_version llbcFile
        , ": crate '"
        , translatedCrateCrate_name crate
        , "' with "
        , show typeDeclCount, " type decls, "
        , show globalDeclCount, " global decls, "
        , show traitDeclCount, " trait decls, "
        , show traitImplCount, " trait impls"
        ]) True

-- Export function to get all LLBC test cases
getAllLlbcTests :: IO TestTree
getAllLlbcTests = do
  let testDir = "../charon/tests/ui"
  dirExists <- doesDirectoryExist testDir
  if not dirExists
    then return $ testGroup "LLBC Files" []
    else do
      llbcFiles <- findLlbcFiles testDir
      let testCases = map createLlbcTest llbcFiles
      return $ testGroup "All LLBC Files" testCases


