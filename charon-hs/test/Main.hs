module Main (main) where

import Test.Tasty
import qualified Test.Deserialize

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Charon Haskell Tests"
  [ Test.Deserialize.tests
  ]
