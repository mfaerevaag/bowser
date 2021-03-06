module Bowser.Test.Engine (engineTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Bowser.Test.Engine.Expr
import Bowser.Test.Engine.Stmt
import Bowser.Test.Engine.Type
import Bowser.Test.Engine.Error

engineTests = testGroup "Engine"
  [
    exprTests
  , stmtTests
  , typeTests
  , errorTests
  ]
