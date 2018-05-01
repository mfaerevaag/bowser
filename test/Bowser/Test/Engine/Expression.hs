module Bowser.Test.Engine.Expression (exprTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Bowser.Test.Helper

import Bowser.Environment

exprTests = testGroup "Expression"
  [
    arithTests
  ]

arithTests = testGroup "Arithmetic"
  [
    testCase "add" $ t (JSNumber 3.0) "1 + 2"
  , testCase "subtract" $ t (JSNumber (-1.0)) "1 - 2"
  , testCase "concat" $ t (JSString "foobar") "'foo' + 'bar'"
  , testCase "cast 1" $ t (JSString "1.0bar") "1 + 'bar'"
  , testCase "cast 2" $ t (JSString "foo2.0") "'foo' + 2"
  , testCase "mult" $ t (JSNumber 21.0) "3 * 7"
  , testCase "div" $ t (JSNumber 7.0) "42 / 6"
  ]
