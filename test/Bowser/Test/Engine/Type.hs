module Bowser.Test.Engine.Type (typeTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Bowser.Types

typeTests = testGroup "Expr"
  [
    boolTests
  ]

boolTests = testGroup "Value to Bool"
  [
    testCase "Undefined" $ False @?= (valueToBool (JSUndefined))
  , testCase "Null" $ False @?= (valueToBool (JSNull))
  , testCase "Bool True" $ True @?= (valueToBool (JSBoolean True))
  , testCase "Bool False" $ False @?= (valueToBool (JSBoolean False))
  , testCase "Number 0" $ False @?= (valueToBool (JSNumber 0))
  , testCase "Number -1" $ True @?= (valueToBool (JSNumber (-1)))
  , testCase "Number 1" $ True @?= (valueToBool (JSNumber 1))
  , testCase "String ''" $ False @?= (valueToBool (JSString ""))
  , testCase "String 'foo'" $ True @?= (valueToBool (JSString "foo"))
  , testCase "Object {}" $ True @?= (valueToBool emptyObject)
  ]
