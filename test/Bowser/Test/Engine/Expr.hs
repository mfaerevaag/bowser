module Bowser.Test.Engine.Expr (exprTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Tainted

import Bowser.Test.Helper
import Bowser.Types

exprTests = testGroup "Expr"
  [
    arithTests
  , logTests
  ]

arithTests = testGroup "Arithmetic"
  [
    testCase "add" $ t (JSNumber (Clean 3.0)) "1 + 2"
  , testCase "sub" $ t (JSNumber (Clean (-1.0))) "1 - 2"
  , testCase "neg" $ t (JSNumber (Clean (-1.0))) "-1"
  , testCase "concat" $ t (JSString (Clean "foobar")) "'foo' + 'bar'"
  , testCase "cast int to string" $ t (JSString (Clean "1.0bar")) "1 + 'bar'"
  , testCase "cast int to string" $ t (JSString (Clean "foo2.0")) "'foo' + 2"
  , testCase "mult" $ t (JSNumber (Clean 21.0)) "3 * 7"
  , testCase "div" $ t (JSNumber (Clean 7.0)) "42 / 6"
  ]

logTests = testGroup "Logical"
  [
    testCase "not" $ t (JSBoolean (Clean False)) "! true"
  , testCase "not" $ t (JSBoolean (Clean True)) "!false"
  , testCase "and" $ t (JSBoolean (Clean True)) "true && true"
  , testCase "and" $ t (JSBoolean (Clean False)) "false && true"
  , testCase "or" $ t (JSBoolean (Clean True)) "true || false"
  , testCase "or" $ t (JSBoolean (Clean False)) "false || false"
  , testCase "eq" $ t (JSBoolean (Clean True)) "1 == 1"
  , testCase "eq" $ t (JSBoolean (Clean False)) "1 == 2"
  , testCase "ne" $ t (JSBoolean (Clean True)) "1 != 2"
  , testCase "ne" $ t (JSBoolean (Clean False)) "1 != 1"
  , testCase "lt" $ t (JSBoolean (Clean True)) "1 < 2"
  , testCase "lt" $ t (JSBoolean (Clean False)) "2 < 2"
  , testCase "le" $ t (JSBoolean (Clean True)) "1 <= 1"
  , testCase "le" $ t (JSBoolean (Clean False)) "1 <= 0"
  , testCase "gt" $ t (JSBoolean (Clean True)) "2 > 1"
  , testCase "gt" $ t (JSBoolean (Clean False)) "2 > 2"
  , testCase "ge" $ t (JSBoolean (Clean True)) "1 >= 1"
  , testCase "ge" $ t (JSBoolean (Clean False)) "0 >= 1"
  , testCase "ternary true" $ t (JSBoolean (Clean True)) "true ? true : false"
  , testCase "ternary false" $ t (JSBoolean (Clean False)) "false ? true : false"
  ]
