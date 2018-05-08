module Bowser.Test.Engine.Stmt (stmtTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Tainted

import Bowser.Test.Helper
import Bowser.Types

stmtTests = testGroup "Stmt"
  [
    ifTests
  , breakTests
  ]

ifTests = testGroup "If"
  [
    testCase "if false" $ t (JSUndefined) "if (false) true"
  , testCase "if true" $ t (JSBoolean (Clean True)) "if (true) true"
  , testCase "if true" $ t (JSUndefined) "if (undefined) true"
  , testCase "if true then first branch" $ t (JSBoolean (Clean True)) "if (true) true else false"
  , testCase "if false then second branch" $ t (JSBoolean (Clean False)) "if (false) true else false"
  ]

breakTests = testGroup "Control flow"
  [
    testCase "while break" $ t (JSNumber (Clean 1)) "var i = 0; while (i < 3) { i += 1; break; } i"
  , testCase "while continue" $ t (JSNumber (Clean 3)) "var i = 0; while (i < 3) { i += 1; continue; break; } i"
  , testCase "function return" $ t (JSNumber (Clean 1)) "var i = 0; function a() { i += 1; return; } a(); i"
  , testCase "function recursive return" $ t (JSNumber (Clean 34)) "var fib = function(num) { \
                                                                   \ if (num <= 1) return num; \
                                                                   \ return fib(num - 1) + fib(num - 2); \
                                                                   \ } \
                                                                   \ fib(9)"
  ]
