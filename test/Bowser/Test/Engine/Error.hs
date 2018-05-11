module Bowser.Test.Engine.Error (errorTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Bowser.Engine.Error

import Bowser.Test.Helper

errorTests = testGroup "Error"
  [
    testCase "step threshold" $ terr (EStep 1000) "while (true) {}"
  ]
