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

runJs :: JSAst -> Value
runJs ast = runEngine $ evalAst emptyEnv ast

type Engine a = Identity a

runEngine :: Engine a -> a
runEngine ev = runIdentity ev

evalAst :: Env -> JSAst -> Engine Value
evalAst env (JSAstProgram stmts _) = evalStmt env $ head stmts
evalAst _ _ = error "not implemented"

evalStmt :: Env -> JSStatement -> Engine Value
evalStmt env (JSExpressionStatement expr _) = evalExpr env expr
evalStmt env _ = error "not implemented"

evalExpr :: Env -> JSExpression -> Engine Value
evalExpr env (JSDecimal _ i) = return $ JSInt (read i)
evalExpr env (JSExpressionBinary e1 op e2) = do
  JSInt i1 <- evalExpr env e1
  JSInt i2 <- evalExpr env e2
  return $ JSInt (i1 + i2)
evalExpr env _ = error "not implemented"
