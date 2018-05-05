module Bowser.Test.Engine (engineTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Bowser.Test.Engine.Expr
import Bowser.Test.Engine.Stmt

engineTests = testGroup "Engine"
  [
    exprTests
  , stmtTests
  ]
