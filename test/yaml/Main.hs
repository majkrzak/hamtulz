module Main where

import Data.Empty (empty)
import Data.Log
import Data.Valid (valid)
import Data.Yaml.Helper (parseYaml)
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "YAML"
    [ testGroup
        "parses"
        [ HU.testCase "Int" $ parseYaml "123" @?= (Just (123 :: Int)),
          HU.testCase "Operator Empty" $ parseYaml "{}" @?= Just (empty :: Operator)
        ]
    ]
