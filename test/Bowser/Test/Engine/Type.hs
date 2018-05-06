module Bowser.Test.Engine.Type (typeTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Tainted

import Bowser.Types

typeTests = testGroup "Expr"
  [
    boolTests
  ]

boolTests = testGroup "Value to Bool"
  [
    testCase "Undefined" $ False @?= (valueToBool (JSUndefined))
  , testCase "Null" $ False @?= (valueToBool (JSNull))
  , testCase "Bool True" $ True @?= (valueToBool (JSBoolean (Clean True)))
  , testCase "Bool False" $ False @?= (valueToBool (JSBoolean (Clean False)))
  , testCase "Number 0" $ False @?= (valueToBool (JSNumber (Clean 0)))
  , testCase "Number -1" $ True @?= (valueToBool (JSNumber (Clean (negate 1))))
  , testCase "Number 1" $ True @?= (valueToBool (JSNumber (Clean 1)))
  , testCase "String ''" $ False @?= (valueToBool (JSString (Clean "")))
  , testCase "String 'foo'" $ True @?= (valueToBool (JSString (Clean "foo")))
  , testCase "Object {}" $ True @?= (valueToBool emptyObject)
  ]
