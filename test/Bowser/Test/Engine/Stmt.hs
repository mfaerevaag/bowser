module Bowser.Test.Engine.Stmt (stmtTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Bowser.Test.Helper

import Bowser.Types

stmtTests = testGroup "Stmt"
  [
    ifTests
  ]

ifTests = testGroup "If"
  [
    testCase "if false" $ t (JSUndefined) "if (false) true"
  , testCase "if true" $ t (JSBoolean True) "if (true) true"
  , testCase "if true" $ t (JSUndefined) "if (undefined) true"
  , testCase "if true then first branch" $ t (JSBoolean True) "if (true) {return true;} else {return false;}"
  , testCase "if false then second branch" $ t (JSBoolean False) "if (false) {return true;} else {return false;}"
  ]
