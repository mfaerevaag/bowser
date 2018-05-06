module Bowser.Engine.Interp
  (eval) where

import Data.Maybe
import Data.Tainted
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Bowser.AST
import Bowser.Types
import Bowser.GlobalObject
import Bowser.Engine.Helper
import Bowser.Engine.Error
import Bowser.Engine.Monad

-- engine

eval ast threshold = runEngine (newState threshold newGlobalObject) (evalAst ast)

-- utility

call :: Ident -> [JSExpression] -> Engine Value
call id args = do
  func <- lookupScope id >>= maybe ((throwError . ERef) ("undefined function " ++ id)) return
  pairs <- liftM (zip ((params . native) func)) (sequence (map evalExpr args))
  -- add arguments
  pushScope
  mapM_ (uncurry $ insertScope) pairs
  -- run
  withCont CReturn $ do
    val <- evalStmt . code . native $ func
    return val

-- eval

evalAst :: JSAst -> Engine Value
evalAst (JSAstProgram stmts _) = liftM last $ mapM evalStmt stmts
evalAst x = throwError . ETodo $ "ast " ++ show x

-- statement

evalStmt :: JSStatement -> Engine Value
evalStmt stmt = do
  case stmt of

    -- block
    JSStatementBlock _ block _ _ -> liftM last $ mapM evalStmt block

    -- return
    JSReturn _ me _ -> case me of
      Nothing -> return JSUndefined
      Just e -> evalExpr e >>= returnCont CReturn

    -- variable
    JSVariable _ clist _ -> do
      f clist
      return JSUndefined
      where
        f JSLNil = return ()
        f (JSLOne (JSVarInitExpression (JSIdentifier _ id) init)) =
          evalInit init >>= insertScope id
        f (JSLCons clist' _ (JSVarInitExpression (JSIdentifier _ id) init)) =
          evalInit init >>= insertScope id >> f clist'
        evalInit (JSVarInit _ expr) = evalExpr expr
        evalInit (JSVarInitNone) = return JSUndefined

    -- assign
    JSAssignStatement lhs op rhs _ -> do
      val <- case op of
        JSAssign _ -> evalExpr rhs
        JSPlusAssign _ -> evalExpr (JSExpressionBinary lhs (JSBinOpPlus JSNoAnnot) rhs)
        JSMinusAssign _ -> evalExpr (JSExpressionBinary lhs (JSBinOpMinus JSNoAnnot) rhs)
        JSTimesAssign _ -> evalExpr (JSExpressionBinary lhs (JSBinOpTimes JSNoAnnot) rhs)
        JSDivideAssign _ -> evalExpr (JSExpressionBinary lhs (JSBinOpDivide JSNoAnnot) rhs)
      case lhs of
        JSIdentifier _ id -> updateScope id val >> return JSUndefined
        JSMemberDot e@(JSIdentifier _ id) _ (JSIdentifier _ mem) -> do
          -- check exists
          case (return (evalExpr e)) of
            Nothing -> throwError . ERef $ "undefined variable " ++ id
            Just _ -> return ()
          updateScopeWith id mem val >> return JSUndefined

    -- expression
    JSExpressionStatement expr _ -> evalExpr expr

    -- func
    JSFunction _ (JSIdentName _ id) _ clist _ (JSBlock _ stmt _) _ ->
      insertScope id (newFunc (Just id) params code) >> return JSUndefined
      where
        params = map (\(JSIdentName _ id) -> id) (consumeCommaList clist)
        code = (JSStatementBlock JSNoAnnot stmt JSNoAnnot JSSemiAuto)

    -- call
    JSMethodCall (JSIdentifier _ "print") _ clist _ _ -> do
      mapM_ (\e -> do
                res <- evalExpr e
                liftIO . print $ res
            ) (consumeCommaList clist)
      return JSUndefined
    JSMethodCall (JSIdentifier _ id) _ clist _ _ -> call id (consumeCommaList clist)

    -- if
    JSIf _ _ cond _ stmt -> ifM (liftM valueToBool (evalExpr cond))
                            (evalStmt stmt)
                            (return JSUndefined)
    JSIfElse _ _ cond _ s1 _ s2 -> ifM (liftM valueToBool (evalExpr cond))
                                   (evalStmt s1)
                                   (evalStmt s2)

    -- continuations
    JSBreak _ _ _ -> returnCont CBreak JSUndefined
    JSContinue _ _ _ -> returnCont CContinue JSUndefined

    -- while
    JSWhile _ _ cond _ block -> withCont CBreak (f JSUndefined)
      where f lastValue = do
              c <- evalExpr cond
              if valueToBool c
                then do
                  value <- withCont CContinue (evalStmt block)
                  f value
                else return lastValue

    x -> throwError . ETodo $ "stmt " ++ show x

-- expression

evalExpr :: JSExpression -> Engine Value
evalExpr expr = do
  incState
  case expr of

    -- parens
    JSExpressionParen _ e _ -> evalExpr e

    -- number
    JSDecimal _ s -> return $ JSNumber (Clean (read s))

    -- ident
    JSIdentifier _ id -> lookupScope id >>= maybe ((throwError . ERef) ("unbound variable " ++ id)) return

    -- string literal
    JSStringLiteral _ s -> return $ JSString (Clean (strip s))

    -- other literal
    JSLiteral _ s -> case s of
      "this" -> getThis
      "null" -> return JSNull
      "true" -> return $ JSBoolean (Clean True)
      "false" -> return $ JSBoolean (Clean False)
      _ -> throwError . ETodo $ "literal " ++ s

    -- object literal
    JSObjectLiteral _ clist _ -> do
      pairs <- sequence $ map (\(JSPropertyNameandValue (JSPropertyIdent _ id) _ [e]) -> do
                                  val <- evalExpr e
                                  return (id, val)
                              ) (consumeCommaTrailingList clist)
      return $ newObject pairs

    -- member
    JSMemberDot e1 _ (JSIdentifier _ id) -> do
      obj <- evalExpr e1
      liftM (fromMaybe JSUndefined) $ lookupScopeWith obj id
    JSMemberSquare e1 _ e2 _ -> do
      obj <- evalExpr e1
      i <- evalExpr e2
      case (obj, i) of
        (JSObject { tab = tab }, JSString s) -> liftM (fromMaybe JSUndefined) (lookupScopeWith obj (extractTaint s)) -- TODO: taint
        (JSString s, JSNumber n) -> return $ case s of
          Clean s -> JSString . Clean $ [s!!(floor (extractTaint n))]
          Dirty s -> JSString . Dirty $ [s!!(floor (extractTaint n))]

    -- ternary
    JSExpressionTernary cond _ e1 _ e2 -> ifM (liftM valueToBool (evalExpr cond))
                                          (evalExpr e1)
                                          (evalExpr e2)

    -- call
    JSMemberExpression (JSIdentifier _ id) _ clist _ -> call id (consumeCommaList clist)

    -- func literal
    JSFunctionExpression _ id _ clist _ (JSBlock _ stmt _) -> return $ newFunc name params code
      where
        params = map (\(JSIdentName _ id) -> id) (consumeCommaList clist)
        name = case id of
          JSIdentName _ s -> Just s
          JSIdentNone -> Nothing
        code = (JSStatementBlock JSNoAnnot stmt JSNoAnnot JSSemiAuto)

    -- unary expression
    JSUnaryExpression op e -> do
      val <- evalExpr e
      case op of
        JSUnaryOpMinus _ -> case val of
          JSNumber n -> return $ JSNumber $ liftM (negate) n
          _ -> throwError . EType $ err op
        JSUnaryOpNot _ -> case val of
          JSNumber n -> return $ JSBoolean $ liftM (not) (f val n)
          JSBoolean b -> return $ JSBoolean $ liftM (not) (f val b)
          JSString s -> return $ JSBoolean $ liftM (not) (f val s)
          x -> return $ JSBoolean $ Clean (not (valueToBool x))
          where
            f x v = case v of
              Clean _ -> Clean $ valueToBool x
              Dirty _ -> Dirty $ valueToBool x
        _ -> throwError . ETodo $ "operator " ++ show op
      where
        err op' = "type error: unary operator '" ++ show op' ++ "' got unexpected arg"

    -- binary expression
    JSExpressionBinary e1 op e2 -> do
      e1' <- evalExpr e1
      e2' <- evalExpr e2
      case op of
        -- addition
        JSBinOpPlus _ -> case (e1', e2') of
          (JSNumber i1, JSNumber i2) -> return $ JSNumber $ liftM2 (+) i1 i2
          (JSNumber s1, JSString s2) -> return $ JSString $ liftM2 (++) (liftM (show) s1) s2
          (JSString s1, JSNumber s2) -> return $ JSString $ liftM2 (++) s1 (liftM (show) s2)
          (JSString s1, JSString s2) -> return $ JSString $ liftM2 (++) s1 s2
          _ -> throwError . EType $ err op
        -- subtraction
        JSBinOpMinus _ -> case (e1', e2') of
          (JSNumber i1, JSNumber i2) -> return $ JSNumber $ liftM2 (-) i1 i2
          _ -> throwError . EType $ err op
        -- multiplication
        JSBinOpTimes _ -> case (e1', e2') of
          (JSNumber i1, JSNumber i2) -> return $ JSNumber $ liftM2 (*) i1 i2
          _ -> throwError . EType $ err op
        -- division
        JSBinOpDivide _ -> case (e1', e2') of
          (JSNumber i1, JSNumber i2) -> return $ JSNumber $ liftM2 (/) i1 i2
          _ -> throwError . EType $ err op
        -- and
        JSBinOpAnd _ -> case (e1', e2') of
          (JSBoolean b1, JSBoolean b2) -> return $ JSBoolean $ liftM2 (&&) b1 b2
          _ -> throwError . EType $ err op
        -- or
        JSBinOpOr _ -> case (e1', e2') of
          (JSBoolean b1, JSBoolean b2) -> return $ JSBoolean $ liftM2 (||) b1 b2
          _ -> throwError . EType $ err op
        -- equal
        JSBinOpEq _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ liftM2 (==) n1 n2
          _ -> throwError . EType $ err op
        -- not equal
        JSBinOpNeq _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ liftM2 (/=) n1 n2
          _ -> throwError . EType $ err op
        -- less than
        JSBinOpLt _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ liftM2 (<) n1 n2
          _ -> throwError . EType $ err op
        -- less or equal
        JSBinOpLe _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ liftM2 (<=) n1 n2
          _ -> throwError . EType $ err op
        -- greater than
        JSBinOpGt _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ liftM2 (>) n1 n2
          _ -> throwError . EType $ err op
        -- greater or equal
        JSBinOpGe _ -> case (e1', e2') of
          (JSNumber n1, JSNumber n2) -> return $ JSBoolean $ liftM2 (>=) n1 n2
          _ -> throwError . EType $ err op
        _ -> throwError . ETodo $ "not implemented operator: " ++ show op
      where
        err op' = "binary operator '" ++ show op' ++ "' got unexpected args"

    -- expression postfix
    JSExpressionPostfix e op -> case op of
      JSUnaryOpIncr _ -> evalExpr (JSExpressionBinary e (JSBinOpPlus JSNoAnnot) (JSDecimal JSNoAnnot "1"))
      JSUnaryOpDecr _ -> evalExpr (JSExpressionBinary e (JSBinOpMinus JSNoAnnot) (JSDecimal JSNoAnnot "1"))
      _ -> throwError . ETodo $ "post-fix operator " ++ show op

    -- otherwise
    x -> throwError . ETodo $ "expr " ++ show x
