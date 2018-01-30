module Bowser.JS.Engine
  ( runJs
  ) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Bowser.JS.AST
import Bowser.JS.Environment

runJs ast = runEngine emptyEnv 0 (evalAst ast)

-- TODO: state as type
type Engine a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a

runEngine :: Env -> Integer -> Engine a -> IO ((Either String a, [String]), Integer)
runEngine env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st

-- The state is the number of steps (or ticks) in evaluating an expression
tick :: (Num s, MonadState s m) => m()
tick = do
  st <- get
  put (st + 1)

evalAst :: JSAst -> Engine Value
evalAst (JSAstProgram stmts _) = evalStmt $ head stmts
evalAst _ = error "not implemented"

evalStmt :: JSStatement -> Engine Value
evalStmt (JSExpressionStatement expr _) = evalExpr expr
evalStmt _ = error "not implemented"

evalExpr :: JSExpression -> Engine Value
evalExpr (JSDecimal _ s) = do
  tick
  return $ JSInt (read s)
evalExpr (JSExpressionBinary e1 op e2) = do
  tick
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (JSInt i1, JSInt i2) -> return $ JSInt (i1 + i2)
    _ -> throwError "type error: addition expected ints"
evalExpr _ = error "not implemented"
