module Bowser.Test.Engine.Expr (exprTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Bowser.Test.Helper

import Bowser.Types

exprTests = testGroup "Expr"
  [
    arithTests
  , logTests
  ]

arithTests = testGroup "Arithmetic"
  [
    testCase "add" $ t (JSNumber 3.0) "1 + 2"
  , testCase "sub" $ t (JSNumber (-1.0)) "1 - 2"
  , testCase "neg" $ t (JSNumber (-1.0)) "-1"
  , testCase "concat" $ t (JSString "foobar") "'foo' + 'bar'"
  , testCase "cast int to string" $ t (JSString "1.0bar") "1 + 'bar'"
  , testCase "cast int to string" $ t (JSString "foo2.0") "'foo' + 2"
  , testCase "mult" $ t (JSNumber 21.0) "3 * 7"
  , testCase "div" $ t (JSNumber 7.0) "42 / 6"
  ]

logTests = testGroup "Logical"
  [
    testCase "not" $ t (JSBoolean False) "! true"
  , testCase "not" $ t (JSBoolean True) "!false"
  , testCase "and" $ t (JSBoolean True) "true && true"
  , testCase "and" $ t (JSBoolean False) "false && true"
  , testCase "or" $ t (JSBoolean True) "true || false"
  , testCase "or" $ t (JSBoolean False) "false || false"
  , testCase "eq" $ t (JSBoolean True) "1 == 1"
  , testCase "eq" $ t (JSBoolean False) "1 == 2"
  , testCase "ne" $ t (JSBoolean True) "1 != 2"
  , testCase "ne" $ t (JSBoolean False) "1 != 1"
  , testCase "lt" $ t (JSBoolean True) "1 < 2"
  , testCase "lt" $ t (JSBoolean False) "2 < 2"
  , testCase "le" $ t (JSBoolean True) "1 <= 1"
  , testCase "le" $ t (JSBoolean False) "1 <= 0"
  , testCase "gt" $ t (JSBoolean True) "2 > 1"
  , testCase "gt" $ t (JSBoolean False) "2 > 2"
  , testCase "ge" $ t (JSBoolean True) "1 >= 1"
  , testCase "ge" $ t (JSBoolean False) "0 >= 1"
  , testCase "ternary true" $ t (JSBoolean True) "true ? true : false"
  , testCase "ternary false" $ t (JSBoolean False) "false ? true : false"
  ]
