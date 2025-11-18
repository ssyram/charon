module Main (main) where

import Test.Tasty
import qualified Test.Deserialize

main :: IO ()
main = do
  -- Get dynamic LLBC tests
  llbcTests <- Test.Deserialize.getAllLlbcTests
  
  -- Run all tests
  defaultMain $ testGroup "Charon Haskell Tests"
    [ Test.Deserialize.tests
    , llbcTests
    ]
