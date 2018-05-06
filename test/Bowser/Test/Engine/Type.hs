module Bowser.Test.Engine.Type (typeTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Bowser.Test.Helper

import Bowser.Types

typeTests = testGroup "Expr"
  [
    boolTests
  ]

boolTests = testGroup "Value to Bool"
  [
    testCase "Number 0" $ t (JSBoolean False) "0"
  ]
