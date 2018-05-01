module Bowser.Engine
  ( eval
  ) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer

import Bowser.AST
import Bowser.Environment
import Bowser.Helper

-- utility

eval ast = runEngine emptyEnv 0 (evalAst ast)

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
evalExpr (JSExpressionParen _ e _) = do
  incState
  evalExpr e
evalExpr (JSDecimal _ s) = do
  incState
  return $ JSNumber (read s)
evalExpr (JSLiteral _ s) = do
  incState
  case s of
    "true" -> return $ JSBoolean True
    "false" -> return $ JSBoolean False
    _ -> throwError ("unknown literal: " ++ s)
evalExpr (JSStringLiteral _ s) = do
  incState
  return $ JSString (strip s)
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
  case op of
    -- addition
    JSBinOpPlus _ -> case (e1', e2') of
      (JSNumber i1, JSNumber i2) -> return $ JSNumber (i1 + i2)
      (JSNumber s1, JSString s2) -> return $ JSString ((show s1) ++ s2)
      (JSString s1, JSNumber s2) -> return $ JSString (s1 ++ (show s2))
      (JSString s1, JSString s2) -> return $ JSString (s1 ++ s2)
      _ -> throwError $ err op
    -- subtraction
    JSBinOpMinus _ -> case (e1', e2') of
      (JSNumber i1, JSNumber i2) -> return $ JSNumber (i1 - i2)
      _ -> throwError $ err op
    -- multiplication
    JSBinOpTimes _ -> case (e1', e2') of
      (JSNumber i1, JSNumber i2) -> return $ JSNumber (i1 * i2)
      _ -> throwError $ err op
    -- division
    JSBinOpDivide _ -> case (e1', e2') of
      (JSNumber i1, JSNumber i2) -> return $ JSNumber (i1 / i2)
      _ -> throwError $ err op
    -- and
    JSBinOpAnd _ -> case (e1', e2') of
      (JSBoolean b1, JSBoolean b2) -> return $ JSBoolean (b1 && b2)
      _ -> throwError $ err op
    -- or
    JSBinOpOr _ -> case (e1', e2') of
      (JSBoolean b1, JSBoolean b2) -> return $ JSBoolean (b1 || b2)
      _ -> throwError $ err op
    -- equal
    JSBinOpEq _ -> case (e1', e2') of
      (JSNumber n1, JSNumber n2) -> return $ JSBoolean (n1 == n2)
      _ -> throwError $ err op
    -- not equal
    JSBinOpNeq _ -> case (e1', e2') of
      (JSNumber n1, JSNumber n2) -> return $ JSBoolean (n1 /= n2)
      _ -> throwError $ err op
    -- less than
    JSBinOpLt _ -> case (e1', e2') of
      (JSNumber n1, JSNumber n2) -> return $ JSBoolean (n1 < n2)
      _ -> throwError $ err op
    -- less or equal
    JSBinOpLe _ -> case (e1', e2') of
      (JSNumber n1, JSNumber n2) -> return $ JSBoolean (n1 <= n2)
      _ -> throwError $ err op
    -- greater than
    JSBinOpGt _ -> case (e1', e2') of
      (JSNumber n1, JSNumber n2) -> return $ JSBoolean (n1 > n2)
      _ -> throwError $ err op
    -- greater or equal
    JSBinOpGe _ -> case (e1', e2') of
      (JSNumber n1, JSNumber n2) -> return $ JSBoolean (n1 >= n2)
      _ -> throwError $ err op
    _ -> throwError ("not implemented operator: " ++ (show op))
  where
    err op' = ("type error: '" ++ (show op') ++ "' operator unexpected args")
evalExpr x = throwError ("not implemented expr: " ++ (show x))

evalVarInitializer :: JSVarInitializer -> Engine Value
evalVarInitializer (JSVarInit _ expr) = evalExpr expr
evalVarInitializer (JSVarInitNone) = return JSUndefined
