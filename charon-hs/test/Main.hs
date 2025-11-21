module Main (main) where

import Test.Tasty ( defaultMain, testGroup )
import qualified Test.Deserialize
import qualified Test.Printing
import qualified Test.Print

main :: IO ()
main = do
  -- Get dynamic LLBC tests
  llbcTests <- Test.Deserialize.getAllLlbcTests
  
  -- Run all tests
  defaultMain $ testGroup "Charon Haskell Tests"
    [ Test.Deserialize.tests
    , llbcTests
    , Test.Printing.tests
    , Test.Print.tests
    ]
