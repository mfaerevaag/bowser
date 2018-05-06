module Bowser.Engine
  ( eval
  ) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer

import Bowser.AST
import Bowser.Types
import Bowser.GlobalObject
import Bowser.Helper


-- engine

eval ast threshold = runEngine newGlobalObject (newState threshold) (evalAst ast)

type State = (Integer, Maybe Integer)
             -- Environment    Exception       Logging           State
type Engine a = ReaderT Scope (ExceptT String (WriterT [String] (StateT State IO))) a

runEngine :: Scope -> State -> Engine a -> IO ((Either String a, [String]), State)
runEngine env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

-- utility

newState :: Maybe Integer -> State
newState threshold = (0, threshold)

-- incState :: (Num s, MonadState s m) => m()
incState = do
  (st, threshold) <- get
  -- put (st + 1)
  case threshold of
    Just t -> if (st > t)
      then throwError "eval stopped: step threshold reached"
      else put ((st + 1), threshold)
    Nothing -> put ((st + 1), threshold)

-- logScope = do
--   st <- get
--   scope <- ask
--   tell [(show st) ++ ": " ++ (drop 9 (show scope))]

-- eval

evalAst :: JSAst -> Engine Value
evalAst (JSAstProgram stmts _) = evalStmt stmts
evalAst x = throwError ("not implemented ast: " ++ (show x))

-- statement

evalStmt :: [JSStatement] -> Engine Value
evalStmt ss = do
  -- logScope
  scope <- ask
  case ss of

    -- nothing
    [] -> return $ JSUndefined

    -- block
    (JSStatementBlock _ ss _ _):xs -> evalStmt (ss++xs)

    -- return
    (JSReturn _ me _):ss -> case me of
      Nothing -> return JSUndefined
      Just e -> evalExpr e

    -- variable
    (JSVariable _ clist _):ss -> case clist of
      JSLOne (JSVarInitExpression (JSIdentifier _ id) init) -> do
        val <- case init of
          JSVarInit _ expr -> evalExpr expr
          JSVarInitNone -> return JSUndefined
        local (const (insertScope scope id val)) (evalStmt ss)
      JSLCons clist' _ x -> evalStmt ((wrap clist'):(wrap (JSLOne x)):ss)
        where
          wrap x = (JSVariable JSNoAnnot x (JSSemi JSNoAnnot))

    -- assign
    (JSAssignStatement lhs@(JSIdentifier _ id) op rhs _):xs -> do
      val <- case op of
        JSAssign _ -> evalExpr rhs
        JSPlusAssign _ -> evalExpr (JSExpressionBinary lhs (JSBinOpPlus JSNoAnnot) rhs)
        JSMinusAssign _ -> evalExpr (JSExpressionBinary lhs (JSBinOpMinus JSNoAnnot) rhs)
        JSTimesAssign _ -> evalExpr (JSExpressionBinary lhs (JSBinOpTimes JSNoAnnot) rhs)
        JSDivideAssign _ -> evalExpr (JSExpressionBinary lhs (JSBinOpDivide JSNoAnnot) rhs)
      local (const (insertScope scope id val)) (evalStmt xs)

    -- expression
    (JSExpressionStatement expr _):xs -> do
      val <- evalExpr expr
      case xs of
        [] -> return val
        xs -> evalStmt xs

    -- func
    (JSFunction _ (JSIdentName _ id) _ clist _ (JSBlock _ ss _) _):xs -> do
      local (const (insertScope scope id (newFunc (Just id) params ss))) (evalStmt xs)
      where
        params = map (\(JSIdentName _ id) -> id) (consumeCommaList clist)

    -- call
    (JSMethodCall (JSIdentifier _ "print") _ clist _ _):xs -> do
      mapM_ (\e -> do
                res <- evalExpr e
                liftIO $ print (show res)
            ) (consumeCommaList clist)
      evalStmt xs
    (JSMethodCall (JSIdentifier _ id) _ clist _ _):xs -> do
      func <- case lookupScope scope id of
        Nothing -> throwError ("undefined function: " ++ id)
        Just val -> return val
      args <- sequence $ map evalExpr (consumeCommaList clist)
      pairs <- return $ zip (params (native func)) args
      local (const (foldr (\(id, val) acc -> (insertScope acc id val)) scope pairs))
        (evalStmt ((code (native func))++ss))

    -- if
    (JSIf _ _ e _ s):xs -> do
      res <- evalExpr e
      evalStmt $ if (valueToBool res) then s:xs else xs
    (JSIfElse _ _ e _ s1 _ s2):xs -> do
      res <- evalExpr e
      evalStmt $ if (valueToBool res) then s1:xs else s2:xs

    -- while
    x@(JSWhile _ _ e _ s):xs -> do
      res <- evalExpr e
      evalStmt $ if (valueToBool res) then s:x:xs else xs

    x -> throwError ("not implemented stmt: " ++ (show x))

-- expression

evalExpr :: JSExpression -> Engine Value
evalExpr expr = do
  incState
  scope <- ask
  case expr of

    -- parens
    JSExpressionParen _ e _ -> evalExpr e

    -- number
    JSDecimal _ s -> return $ JSNumber (read s)

    -- ident
    JSIdentifier _ s -> case lookupScope scope s of
      Nothing -> throwError ("unbound variable: " ++ s)
      Just val -> return val

    -- string literal
    JSStringLiteral _ s -> return $ JSString (strip s)

    -- other literal
    JSLiteral _ s -> case s of
      "this" -> return scope
      "null" -> return JSNull
      "true" -> return $ JSBoolean True
      "false" -> return $ JSBoolean False
      _ -> throwError ("unknown literal: " ++ s)

    -- object literal
    JSObjectLiteral _ clist _ -> do
      pairs <- sequence $ map (\(JSPropertyNameandValue (JSPropertyIdent _ id) _ [e]) -> do
                                  val <- evalExpr e
                                  return (id, val)
                              ) (consumeCommaTrailingList clist)
      return $ newObject pairs

    -- member
    JSMemberDot (JSIdentifier _ id) _ (JSIdentifier _ mem) -> do
      obj <- case lookupScope scope id of
        Nothing -> throwError ("unbound variable: " ++ id)
        Just val -> return val
      val <- case lookupScope obj mem of
        Nothing -> return JSUndefined
        Just val -> return val
      return val

    -- ternary
    JSExpressionTernary ce _ e1 _ e2 -> do
      res <- evalExpr ce
      if (valueToBool res) then (evalExpr e1) else (evalExpr e2)

    -- call
    JSMemberExpression (JSIdentifier _ id) _ clist _ -> do
      func <- case lookupScope scope id of
        Nothing -> throwError ("undefined function: " ++ id)
        Just val -> return val
      args <- sequence $ map evalExpr (consumeCommaList clist)
      pairs <- return $ zip (params (native func)) args
      local (const (foldr (\(id, val) acc -> (insertScope acc id val)) scope pairs))
        (evalStmt (code (native func)))

    -- func literal
    JSFunctionExpression _ id _ clist _ (JSBlock _ ss _) -> return $ newFunc name params ss
      where
        params = map (\(JSIdentName _ id) -> id) (consumeCommaList clist)
        name = case id of
          JSIdentName _ s -> Just s
          JSIdentNone -> Nothing

    -- unary expression
    JSUnaryExpression op e -> do
      e' <- evalExpr e
      case op of
        JSUnaryOpMinus _ -> case e' of
          JSNumber n -> return $ JSNumber $ negate n
          _ -> throwError $ err op
        JSUnaryOpNot _ -> return $ JSBoolean $ not (valueToBool e')
        _ -> throwError ("not implemented operator: " ++ (show op))
      where
        err op' = ("type error: unary operator '" ++ (show op') ++ "' got unexpected arg")

    -- binary expression
    JSExpressionBinary e1 op e2 -> do
      e1' <- evalExpr e1
      e2' <- evalExpr e2
      case op of
        -- addition
        JSBinOpPlus _ -> case (e1', e2') of
          (JSNumber i1, JSNumber i2) -> return $ JSNumber $ (+) i1 i2
          (JSNumber s1, JSString s2) -> return $ JSString $ (++) (show s1) s2
          (JSString s1, JSNumber s2) -> return $ JSString $ (++) s1 (show s2)
          (JSString s1, JSString s2) -> return $ JSString $ (++) s1 s2
          _ -> throwError $ err op
        -- subtraction
        JSBinOpMinus _ -> case (e1', e2') of
          (JSNumber i1, JSNumber i2) -> return $ JSNumber $ (-) i1 i2
          _ -> throwError $ err op
        -- multiplication
        JSBinOpTimes _ -> case (e1', e2') of
          (JSNumber i1, JSNumber i2) -> return $ JSNumber $ (*) i1 i2
          _ -> throwError $ err op
        -- division
        JSBinOpDivide _ -> case (e1', e2') of
          (JSNumber i1, JSNumber i2) -> return $ JSNumber $ (/) i1 i2
          _ -> throwError $ err op
        -- and
        JSBinOpAnd _ -> case (e1', e2') of
          (JSBoolean b1, JSBoolean b2) -> return $ JSBoolean $ (&&) b1 b2
          _ -> throwError $ err op
        -- or
        JSBinOpOr _ -> case (e1', e2') of
          (JSBoolean b1, JSBoolean b2) -> return $ JSBoolean $ (||) b1 b2
          _ -> throwError $ err op
        -- equal
        JSBinOpEq _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ (==) n1 n2
          _ -> throwError $ err op
        -- not equal
        JSBinOpNeq _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ (/=) n1 n2
          _ -> throwError $ err op
        -- less than
        JSBinOpLt _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ (<) n1 n2
          _ -> throwError $ err op
        -- less or equal
        JSBinOpLe _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ (<=) n1 n2
          _ -> throwError $ err op
        -- greater than
        JSBinOpGt _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ (>) n1 n2
          _ -> throwError $ err op
        -- greater or equal
        JSBinOpGe _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ (>=) n1 n2
          _ -> throwError $ err op
        _ -> throwError ("not implemented operator: " ++ (show op))
      where
        err op' = ("type error: binary operator '" ++ (show op') ++ "' got unexpected args")

    -- expression postfix
    JSExpressionPostfix e op -> case op of
      JSUnaryOpIncr _ -> evalExpr (JSExpressionBinary e (JSBinOpPlus JSNoAnnot) (JSDecimal JSNoAnnot "1"))
      JSUnaryOpDecr _ -> evalExpr (JSExpressionBinary e (JSBinOpMinus JSNoAnnot) (JSDecimal JSNoAnnot "1"))
      _ -> throwError ("not implemented post-fix operator: " ++ (show op))

    -- otherwise
    x -> throwError ("not implemented expr: " ++ (show x))
