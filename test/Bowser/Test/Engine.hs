module Bowser.Test.Engine (engineTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Bowser.Test.Engine.Expr

engineTests = testGroup "Engine"
  [
    exprTests
  ]
