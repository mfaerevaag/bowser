module Bowser.Engine.Monad where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer
import Control.Monad.Cont hiding (Cont)

import Bowser.Types

             -- Exception    -- State      -- Continuation
type Engine a = ExceptT String (StateT State (ContT ((Either String Value), State) IO)) a

runEngine :: State -> Engine Value -> IO ((Either String Value), State)
runEngine st ast = runContT (runStateT (runExceptT ast) st) return

-- state

data State = State { step :: Integer
                   , threshold :: Maybe Integer
                   , scopeStack :: [Scope]
                   , contStack :: [Cont]}
           deriving Show

newState :: Maybe Integer -> Scope -> State
newState th scope = State { step = 0, threshold = th, scopeStack = [scope], contStack = [] }

incState :: Engine ()
incState = do
  -- check step counter
  State { step = st, threshold = th } <- get
  case th of
    Just t -> when (st > t) $
      throwError "eval stopped: step threshold reached"
    _ -> return ()
  -- inc counter
  modify $ \s -> s { step = (st + 1) }

insertScope :: Ident -> Value -> Engine ()
insertScope id val = do
  st@State { scopeStack = scope:rest } <- get
  scope' <- return $ insertObject scope id val
  put $ st { scopeStack = scope':rest }

-- TODO: fix
updateScopeWith :: Ident -> Ident -> Value -> Engine ()
updateScopeWith id mem val =
  modify $ \state@State { scopeStack = stack } ->
             state { scopeStack = (map (\s -> case lookupObject s id of
                                                Just obj -> insertObject s id (insertObject obj mem val)
                                                Nothing -> s
                                       ) stack)}

updateScope :: Ident -> Value -> Engine ()
updateScope id val = do
  state@State { scopeStack = stack } <- get
  case f stack of
    (Nothing, _) -> throwError $ "undefined variable " ++ id
    (Just _, stack') -> put $ state { scopeStack = stack' }
  where
    f [] = (Nothing, [])
    f (s:ss) = case lookupObject s id of
      Just x -> (Just x, ((insertObject s id val):ss))
      Nothing -> case (f ss) of
        (Nothing, ss') -> (Nothing, s:ss')
        (Just x, ss') -> (Just x, s:ss')

lookupScopeWith :: Scope -> Ident -> Engine (Maybe Value)
lookupScopeWith scope id = return $ lookupObject scope id

lookupScope :: Ident -> Engine (Maybe Value)
lookupScope id = do
  State { scopeStack = stack } <- get
  return $ f stack
  where
    f [] = Nothing
    f (s:ss) = case lookupObject s id of
      Just x -> Just x
      Nothing -> f ss

getScope :: Engine Value
getScope = do
  State { scopeStack = stack } <- get
  return $ head stack

getThis :: Engine Value
getThis = do
  State { scopeStack = stack } <- get
  return $ last stack

emptyScope :: Scope
emptyScope = emptyObject

pushScope :: Engine ()
pushScope = modify $ \state@State { scopeStack = stack } ->
                       state { scopeStack = emptyScope:stack }

popScope :: Engine ()
popScope = modify $ \state@State { scopeStack = _:stack } ->
                       state { scopeStack = stack }

-- cont

data Cont = Cont { cType :: ContType
                 , cCont :: Value -> Engine Value
                 , cScope :: [Scope] }

instance Show Cont where
  show Cont { cType = ct, cCont = cc } = "<cont " ++ (show ct) ++ ">"

data ContType = CBreak
              | CContinue
              | CReturn
              deriving Eq

instance Show ContType where
  show CBreak = "break"
  show CContinue = "continue"
  show CReturn = "return"

pushCont :: (Value -> Engine Value) -> ContType -> Engine ()
pushCont cc ct =
  modify $ \state@State { scopeStack = cs, contStack = stack } ->
             state { contStack = (Cont ct cc cs):stack }

popCont :: ContType -> Engine Cont
popCont ct = do
  s@State { contStack = cs } <- get
  case cs of
    [] -> do throwError $ "illegal " ++ show ct ++ " statement"
    (c:cs') -> do
      put $ s { contStack = cs' }
      return c

withCont :: ContType -> Engine Value -> Engine Value
withCont ct proc = do
  callCC $ \cc -> do
    pushCont cc ct
    value <- proc
    popCont ct
    return value

returnCont :: ContType -> Value -> Engine Value
returnCont ct value = do
  Cont { cType = ct', cCont = cc, cScope = cs } <- popCont ct
  if ct' == ct
    then do
      -- modify $ \state -> state { scopeStack = cs }
      when (ct == CReturn) popScope
      cc value
    else returnCont ct value
