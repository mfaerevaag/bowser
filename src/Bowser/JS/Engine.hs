module Bowser.JS.Engine
  ( runJs
  ) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer

import Bowser.JS.AST
import Bowser.JS.Environment

-- utility

runJs ast = runEngine emptyEnv 0 (evalAst ast)

-- engine

type State = Integer
type Engine a = ReaderT Env (ExceptT String (WriterT [String] (StateT State IO))) a

runEngine :: Env -> State -> Engine a -> IO ((Either String a, [String]), State)
runEngine env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

-- state

emptyState :: State
emptyState = 0

incState :: (Num s, MonadState s m) => m()
incState = do
  st <- get
  put (st + 1)

-- interpreter

evalAst :: JSAst -> Engine Value
evalAst (JSAstProgram stmts _) = evalStmt stmts
evalAst x = throwError ("not implemented ast: " ++ (show x))

evalStmt :: [JSStatement] -> Engine Value
evalStmt ((JSVariable _ clist _):xs) = case clist of
  JSLOne (JSVarInitExpression (JSIdentifier _ id) init) -> do
    val <- evalVarInitializer init
    env <- ask
    local (const (insertEnv id val env)) (evalStmt xs)
  otherwise -> throwError "not implemented TODO"
evalStmt ((JSExpressionStatement expr _):xs) = do
  val <- evalExpr expr
  case xs of
    [] -> return val
    xs -> evalStmt xs
evalStmt x = throwError ("not implemented stmt: " ++ (show x))

evalExpr :: JSExpression -> Engine Value
-- terminals
evalExpr (JSDecimal _ s) = do
  incState
  return $ JSNumber (read s)
evalExpr (JSIdentifier _ s) = do
  env <- ask
  case lookupEnv s env of
    Nothing -> throwError ("unbound variable: " ++ s)
    Just val -> return val
-- non-terminals
evalExpr (JSExpressionBinary e1 op e2) = do
  incState
  e1' <- evalExpr e1
  e2' <- evalExpr e2
  case (e1', e2') of
    (JSNumber i1, JSNumber i2) -> return $ JSNumber (i1 + i2)
    _ -> throwError "type error: addition expected ints"
evalExpr x = throwError ("not implemented expr: " ++ (show x))

evalVarInitializer :: JSVarInitializer -> Engine Value
evalVarInitializer (JSVarInit _ expr) = evalExpr expr
evalVarInitializer (JSVarInitNone) = return JSUndefined
