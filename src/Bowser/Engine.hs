module Bowser.Engine
  ( eval
  ) where

import Text.Show.Pretty (ppShow)
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer

import Bowser.AST
import Bowser.Types
import Bowser.Helper

-- utility

eval ast = runEngine emptyScope 0 (evalAst ast)

-- engine

type State = Integer
type Engine a = ReaderT Scope (ExceptT String (WriterT [String] (StateT State IO))) a

runEngine :: Scope -> State -> Engine a -> IO ((Either String a, [String]), State)
runEngine env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

-- state

emptyState :: State
emptyState = 0

incState :: (Num s, MonadState s m) => m()
incState = do
  st <- get
  put (st + 1)

-- eval

evalAst :: JSAst -> Engine Value
evalAst (JSAstProgram stmts _) = evalStmt stmts
evalAst x = throwError ("not implemented ast: " ++ (show x))

-- statement

evalStmt :: [JSStatement] -> Engine Value
evalStmt ss = do
  scope <- ask
  tell [(ppShow scope)]
  case ss of

    -- nothing
    [] -> return $ JSUndefined

    -- variable
    ((JSVariable _ clist _):xs) -> case clist of
      JSLOne (JSVarInitExpression (JSIdentifier _ id) init) -> do
        val <- evalVarInitializer init
        local (const (insertScope id val scope)) (evalStmt xs)
      otherwise -> throwError "not implemented TODO"

    -- expression
    ((JSExpressionStatement expr _):xs) -> do
      val <- evalExpr expr
      case xs of
        [] -> return val
        xs -> evalStmt xs

    x -> throwError ("not implemented stmt: " ++ (show x))

-- expression

evalExpr :: JSExpression -> Engine Value
evalExpr expr = do
  incState
  scope <- ask
  case expr of

    -- parens
    (JSExpressionParen _ e _) -> evalExpr e

    -- number
    (JSDecimal _ s) -> return $ JSNumber (read s)

    -- ident
    (JSIdentifier _ s) -> case lookupScope s scope of
      Nothing -> throwError ("unbound variable: " ++ s)
      Just val -> return val

    -- string literal
    (JSStringLiteral _ s) -> return $ JSString (strip s)

    -- other literal
    (JSLiteral _ s) -> case s of
      "true" -> return $ JSBoolean True
      "false" -> return $ JSBoolean False
      _ -> throwError ("unknown literal: " ++ s)

    -- unary expression
    (JSUnaryExpression op e) -> do
      e' <- evalExpr e
      case op of
        JSUnaryOpMinus _ -> case e' of
          (JSNumber n) -> return $ JSNumber (- n)
          _ -> throwError $ err op
        JSUnaryOpNot _ -> case e' of
          (JSBoolean b) -> return $ JSBoolean (not b)
          _ -> throwError $ err op
        _ -> throwError ("not implemented operator: " ++ (show op))
      where
        err op' = ("type error: unary operator '" ++ (show op') ++ "' got unexpected arg")

    -- binary expression
    (JSExpressionBinary e1 op e2) -> do
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
        err op' = ("type error: binary operator '" ++ (show op') ++ "' got unexpected args")

    -- otherwise
    x -> throwError ("not implemented expr: " ++ (show x))

evalVarInitializer :: JSVarInitializer -> Engine Value
evalVarInitializer (JSVarInit _ expr) = evalExpr expr
evalVarInitializer (JSVarInitNone) = return JSUndefined
