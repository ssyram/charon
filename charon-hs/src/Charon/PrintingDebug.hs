{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Charon.PrintingDebug
Description: Interactive debugging utilities for printing mechanism

This module provides helper functions for interactive debugging of the printing
mechanism during GHCI sessions.
-}
module Charon.PrintingDebug
  ( -- * Interactive testing functions
    compareFile
  , printFile
  , showDiff
  , loadAndPrint
  ) where

import Data.Aeson (eitherDecodeFileStrict)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isPrefixOf)

-- Import our modules
import Charon.Printing
import Generated_Krate (LlbcFile(..), TranslatedCrate(..), FunDecl)
import qualified Generated_Meta as M
import qualified Generated_GAst as G
import qualified Generated_Types as T

-- | Load and print a single LLBC file, comparing with expected output
-- 
-- Usage in stack ghci:
-- >>> :load Charon.PrintingDebug
-- >>> compareFile "foreign-constant"
compareFile :: String -> IO ()
compareFile testName = do
  let basePath = "../charon/tests/ui"
  let llbcPath = basePath </> testName ++ ".llbc"
  let outPath = basePath </> testName ++ ".out"
  
  -- Check files exist
  llbcExists <- doesFileExist llbcPath
  outExists <- doesFileExist outPath
  
  if not llbcExists
    then putStrLn $ "ERROR: LLBC file not found: " ++ llbcPath
    else if not outExists
      then putStrLn $ "ERROR: Out file not found: " ++ outPath
      else do
        -- Load LLBC file
        result <- eitherDecodeFileStrict llbcPath
        case result of
          Left err -> putStrLn $ "ERROR: Failed to decode LLBC file: " ++ err
          Right (LlbcFile _version crate) -> do
            -- Load expected output
            expectedContent <- readFile outPath
            let expectedLines = lines expectedContent
            
            -- Skip header line and extract actual LLBC output
            let llbcOutput = dropWhile (\l -> l == "" || isPrefixOf "#" l) expectedLines
            
            -- Create printing context
            let ctx = emptyCtx { translated = Just crate }
            
            -- Print the crate
            let actualOutput = printCrateDebug ctx crate
            let actualLines = lines actualOutput
            
            -- Show comparison
            putStrLn "========================================="
            putStrLn $ "Comparison for: " ++ testName
            putStrLn "========================================="
            putStrLn ""
            putStrLn "=== EXPECTED OUTPUT ==="
            putStrLn $ unlines $ take 30 llbcOutput
            putStrLn "..."
            putStrLn ""
            putStrLn "=== ACTUAL OUTPUT ==="
            putStrLn $ unlines $ take 30 actualLines
            putStrLn "..."
            putStrLn ""
            putStrLn "========================================="
            putStrLn $ "Expected lines: " ++ show (length llbcOutput)
            putStrLn $ "Actual lines:   " ++ show (length actualLines)
            putStrLn "========================================="

-- | Just print a single LLBC file without comparison
--
-- Usage in stack ghci:
-- >>> printFile "foreign-constant"
printFile :: String -> IO ()
printFile testName = do
  let basePath = "../charon/tests/ui"
  let llbcPath = basePath </> testName ++ ".llbc"
  
  llbcExists <- doesFileExist llbcPath
  
  if not llbcExists
    then putStrLn $ "ERROR: LLBC file not found: " ++ llbcPath
    else do
      result <- eitherDecodeFileStrict llbcPath
      case result of
        Left err -> putStrLn $ "ERROR: Failed to decode LLBC file: " ++ err
        Right (LlbcFile _version crate) -> do
          let ctx = emptyCtx { translated = Just crate }
          putStrLn $ printCrateDebug ctx crate

-- | Show line-by-line diff between expected and actual
--
-- Usage in stack ghci:
-- >>> showDiff "foreign-constant"
showDiff :: String -> IO ()
showDiff testName = do
  let basePath = "../charon/tests/ui"
  let llbcPath = basePath </> testName ++ ".llbc"
  let outPath = basePath </> testName ++ ".out"
  
  llbcExists <- doesFileExist llbcPath
  outExists <- doesFileExist outPath
  
  if not llbcExists
    then putStrLn $ "ERROR: LLBC file not found: " ++ llbcPath
    else if not outExists
      then putStrLn $ "ERROR: Out file not found: " ++ outPath
      else do
        result <- eitherDecodeFileStrict llbcPath
        case result of
          Left err -> putStrLn $ "ERROR: Failed to decode LLBC file: " ++ err
          Right (LlbcFile _version crate) -> do
            expectedContent <- readFile outPath
            let expectedLines = lines expectedContent
            let llbcOutput = dropWhile (\l -> l == "" || isPrefixOf "#" l) expectedLines
            
            let ctx = emptyCtx { translated = Just crate }
            let actualOutput = printCrateDebug ctx crate
            let actualLines = lines actualOutput
            
            -- Show line-by-line diff
            putStrLn "========================================="
            putStrLn $ "Line-by-line diff for: " ++ testName
            putStrLn "========================================="
            
            let maxLines = max (length llbcOutput) (length actualLines)
            let paddedExpected = llbcOutput ++ repeat ""
            let paddedActual = actualLines ++ repeat ""
            
            mapM_ (showLineDiff) $ take maxLines $ zip3 [1..] paddedExpected paddedActual
            
            putStrLn "========================================="
            putStrLn "DIFF LEGEND:"
            putStrLn "  [OK]   Lines match"
            putStrLn "  [DIFF] Lines differ"
            putStrLn "========================================="

-- Helper to show a single line diff
showLineDiff :: (Int, String, String) -> IO ()
showLineDiff (lineNum, expected, actual)
  | expected == actual && not (null expected) = 
      putStrLn $ "[OK]   Line " ++ show lineNum ++ ": " ++ take 60 expected
  | expected /= actual =
      do putStrLn $ "[DIFF] Line " ++ show lineNum ++ ":"
         putStrLn $ "  Expected: " ++ expected
         putStrLn $ "  Actual:   " ++ actual
         putStrLn ""
  | otherwise = return ()

-- | Load LLBC file and return the crate for custom processing
--
-- Usage in stack ghci:
-- >>> crate <- loadAndPrint "foreign-constant"
-- >>> let ctx = emptyCtx { translated = Just crate }
-- >>> printWithCtx ctx someDecl
loadAndPrint :: String -> IO (Maybe TranslatedCrate)
loadAndPrint testName = do
  let basePath = "../charon/tests/ui"
  let llbcPath = basePath </> testName ++ ".llbc"
  
  llbcExists <- doesFileExist llbcPath
  
  if not llbcExists
    then do
      putStrLn $ "ERROR: LLBC file not found: " ++ llbcPath
      return Nothing
    else do
      result <- eitherDecodeFileStrict llbcPath
      case result of
        Left err -> do
          putStrLn $ "ERROR: Failed to decode LLBC file: " ++ err
          return Nothing
        Right (LlbcFile _version crate) -> do
          putStrLn $ "Successfully loaded: " ++ testName
          return $ Just crate

-- Import the print functions from Test.PrintComparison
printCrateDebug :: PrintingCtx -> TranslatedCrate -> String
printCrateDebug ctx crate = unlines
  [ "# Final LLBC before serialization:"
  , ""
  , printDeclsDebug ctx crate
  ]

-- Helper to extract values from Vector
vectorToList :: M.Vector k v -> [v]
vectorToList (M.Vector xs) = xs

-- Print all declarations in a crate
printDeclsDebug :: PrintingCtx -> TranslatedCrate -> String
printDeclsDebug ctx crate = unlines $ concat
  [ map (printTypeDeclDebug ctx) (vectorToList $ translatedcrateTypeDecls crate)
  , map (printFunDeclDebug ctx) (vectorToList $ translatedcrateFunDecls crate)
  , map (printGlobalDeclDebug ctx) (vectorToList $ translatedcrateGlobalDecls crate)
  , map (printTraitDeclDebug ctx) (vectorToList $ translatedcrateTraitDecls crate)
  , map (printTraitImplDebug ctx) (vectorToList $ translatedcrateTraitImpls crate)
  ]

-- Helper functions to print specific declaration types
printTypeDeclDebug :: PrintingCtx -> T.TypeDecl -> String
printTypeDeclDebug ctx decl = printWithCtx ctx decl ++ "\n"

printFunDeclDebug :: PrintingCtx -> FunDecl -> String
printFunDeclDebug ctx decl = printWithCtx ctx decl ++ "\n"

printGlobalDeclDebug :: PrintingCtx -> G.GlobalDecl -> String
printGlobalDeclDebug ctx decl = printWithCtx ctx decl ++ "\n"

printTraitDeclDebug :: PrintingCtx -> G.TraitDecl -> String
printTraitDeclDebug ctx decl = printWithCtx ctx decl ++ "\n"

printTraitImplDebug :: PrintingCtx -> G.TraitImpl -> String
printTraitImplDebug ctx impl = printWithCtx ctx impl ++ "\n"
