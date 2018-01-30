module Bowser.JS.Engine
  ( runJs
  ) where

import Control.Monad.Identity
-- import Control.Monad.Error
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Writer

import Bowser.JS.AST
import Bowser.JS.Environment

runJs ast = runEngine $ evalAst ast

type Engine a = Identity a

runEngine :: Engine a -> a
runEngine ev = runIdentity ev

evalAst :: JSAst -> Engine JSValue
evalAst (JSAstProgram stmts _) = evalStmt $ head stmts
evalAst _ = error "not implemented"

evalStmt :: JSStatement -> Engine JSValue
evalStmt (JSExpressionStatement expr _) = evalExpr expr
evalStmt _ = error "not implemented"

evalExpr :: JSExpression -> Engine JSValue
evalExpr (JSDecimal _ i) = return $ JSInt (read i)
evalExpr (JSExpressionBinary e1 op e2) = do
  JSInt i1 <- evalExpr e1
  JSInt i2 <- evalExpr e2
  return $ JSInt (i1 + i2)
evalExpr _ = error "not implemented"
