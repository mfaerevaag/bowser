module Bowser.JS.Engine
  ( runJs
  ) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer

import Bowser.JS.AST
import Bowser.JS.Environment

-- utility

runJs ast = runEngine emptyEnv 0 (evalAst ast)

-- engine

type State = Integer
type Engine a = ReaderT Env (ErrorT String (WriterT [String] (StateT State IO))) a

runEngine :: Env -> State -> Engine a -> IO ((Either String a, [String]), State)
runEngine env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st

-- state

emptyState :: State
emptyState = 0

incState :: (Num s, MonadState s m) => m()
incState = do
  st <- get
  put (st + 1)

-- interpreter

evalAst :: JSAst -> Engine Value
evalAst (JSAstProgram stmts _) = evalStmt $ head stmts
evalAst _ = throwError "not implemented"

evalStmt :: JSStatement -> Engine Value
evalStmt (JSExpressionStatement expr _) = evalExpr expr
evalStmt _ = throwError "not implemented"

evalExpr :: JSExpression -> Engine Value
evalExpr (JSDecimal _ s) = do
  incState
  return $ JSInt (read s)
evalExpr (JSExpressionBinary e1 op e2) = do
  incState
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (JSInt i1, JSInt i2) -> return $ JSInt (i1 + i2)
    _ -> throwError "type error: addition expected ints"
evalExpr _ = throwError "not implemented"
