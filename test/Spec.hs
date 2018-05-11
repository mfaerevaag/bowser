module Main where

import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit

import Bowser.Test.Helper
import Bowser.Test.Engine

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Bowser Tests"
        [
          -- truthTests
          engineTests
        ]

truthTests = testGroup "Truth"
  [
    testCase "Pass" $
      assertException DivideByZero (evaluate $ 5 `div` 0)
  , testCase "Fail" $
      assertException DivideByZero (evaluate $ 5 `div` 1)
  ]
