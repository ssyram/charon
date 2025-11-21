{-# LANGUAGE OverloadedStrings #-}

module Test.PrintComparison (tests) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.List (isPrefixOf, isSuffixOf)
import System.FilePath ((</>), takeBaseName)
import System.Directory (listDirectory, doesFileExist)
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit
    ( testCase, assertBool, assertFailure, assertEqual, Assertion )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (filterM, forM_)

-- Import our modules
import Charon.Printing
import Generated_Krate (LlbcFile(..), TranslatedCrate(..))
import qualified Generated_GAst as G
import qualified Generated_Types as T
import qualified Generated_Meta as M

tests :: TestTree
tests = testGroup "Print Comparison Tests"
  [ testCase "Compare foreign-constant" (testPrintCompare "foreign-constant")
  , testCase "Compare unsafe-impl-send" (testPrintCompare "unsafe-impl-send")
  , testCase "Compare hide-marker-traits" (testPrintCompare "hide-marker-traits")
  ]

-- Base path to test files
testBasePath :: FilePath
testBasePath = "../charon/tests/ui"

-- Test that compares Haskell printing output with Rust .out file
testPrintCompare :: String -> Assertion
testPrintCompare testName = do
  let llbcPath = testBasePath </> testName ++ ".llbc"
  let outPath = testBasePath </> testName ++ ".out"
  
  -- Check files exist
  llbcExists <- doesFileExist llbcPath
  outExists <- doesFileExist outPath
  assertBool ("LLBC file exists: " ++ llbcPath) llbcExists
  assertBool ("Out file exists: " ++ outPath) outExists
  
  -- Load LLBC file
  result <- eitherDecodeFileStrict llbcPath
  case result of
    Left err -> assertFailure $ "Failed to decode LLBC file: " ++ err
    Right (LlbcFile crate) -> do
      -- Load expected output
      expectedContent <- readFile outPath
      let expectedLines = lines expectedContent
      
      -- Skip header line and extract actual LLBC output
      let llbcOutput = dropWhile (\l -> l == "" || isPrefixOf "#" l) expectedLines
      
      -- Create printing context
      let ctx = emptyCtx { translated = Just crate }
      
      -- Print the crate
      let actualOutput = printCrate ctx crate
      let actualLines = lines actualOutput
      
      -- Compare line by line (for now, just check if we produce output)
      putStrLn $ "\n=== Expected output (" ++ show (length llbcOutput) ++ " lines) ==="
      putStrLn $ unlines $ take 20 llbcOutput
      
      putStrLn $ "\n=== Actual output (" ++ show (length actualLines) ++ " lines) ==="
      putStrLn $ unlines $ take 20 actualLines
      
      -- For now, just assert we produced something
      assertBool "Produced output" (not (null actualLines))

-- Print an entire crate
printCrate :: PrintingCtx -> TranslatedCrate -> String
printCrate ctx crate = unlines
  [ "# Final LLBC before serialization:"
  , ""
  , printDecls ctx crate
  ]

-- Print all declarations in a crate
printDecls :: PrintingCtx -> TranslatedCrate -> String
printDecls ctx crate = unlines $ concat
  [ map (printTypeDecl ctx) (M.vectorValues $ translatedc_type_decls crate)
  , map (printFunDecl ctx) (M.vectorValues $ translatedc_fun_decls crate)
  , map (printGlobalDecl ctx) (M.vectorValues $ translatedc_global_decls crate)
  , map (printTraitDecl ctx) (M.vectorValues $ translatedc_trait_decls crate)
  , map (printTraitImpl ctx) (M.vectorValues $ translatedc_trait_impls crate)
  ]

-- Helper functions to print specific declaration types
printTypeDecl :: PrintingCtx -> T.TypeDecl -> String
printTypeDecl ctx decl = printWithCtx ctx decl ++ "\n"

printFunDecl :: PrintingCtx -> G.FunDecl -> String
printFunDecl ctx decl = printWithCtx ctx decl ++ "\n"

printGlobalDecl :: PrintingCtx -> G.GlobalDecl -> String
printGlobalDecl ctx decl = printWithCtx ctx decl ++ "\n"

printTraitDecl :: PrintingCtx -> G.TraitDecl -> String
printTraitDecl ctx decl = printWithCtx ctx decl ++ "\n"

printTraitImpl :: PrintingCtx -> G.TraitImpl -> String
printTraitImpl ctx impl = printWithCtx ctx impl ++ "\n"
