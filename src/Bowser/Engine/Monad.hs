module Bowser.Engine.Monad where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer
import Control.Monad.Cont hiding (Cont)

import Bowser.Types

type Engine a
  = ReaderT Scope ( -- Environment
    ExceptT String ( -- Exception
        WriterT [String] ( -- Logging
            StateT State ( -- State
                ContT ( -- Continuation
                    (Either String Value, [String]),
                    State)
                  IO)))) a

runEngine :: Scope -> State -> Engine Value -> IO ((Either String Value, [String]), State)
runEngine env st ev = runContT (runStateT (runWriterT (runExceptT (runReaderT ev env))) st) return

-- state

data State = State { step :: Integer
                   , threshold :: Maybe Integer
                   , contStack :: [Cont]}
           deriving Show

newState :: Maybe Integer -> State
newState th = State { step = 0, threshold = th, contStack = [] }

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

-- cont

data Cont = Cont { cType :: ContType
                 , cCont :: Value -> Engine Value
                 , cScope :: Scope }

data ContType = CBreak
              | CContinue
              deriving (Show, Eq)

instance Show Cont where
  show Cont { cType = ct, cCont = cc } = "<cont " ++ (show ct) ++ ">"

pushCont :: (Value -> Engine Value) -> ContType -> Scope -> Engine ()
pushCont cc ct cs = modify $ \state@State { contStack = stack } -> state { contStack = (Cont ct cc cs):stack }

popCont :: Engine Cont
popCont = do
  s@State { contStack = cs } <- get
  case cs of
    [] -> do throwError "contstack is empty"
    (c:cs') -> do
      put $ s { contStack = cs' }
      return c

withCont :: ContType -> Scope -> Engine Value -> Engine Value
withCont ct cs proc = do
  callCC $ \cc -> do
    pushCont cc ct cs
    value <- proc
    scope <- ask
    tell [show scope]
    popCont
    return value

returnCont :: ContType -> Value -> Engine Value
returnCont ct value = do
  Cont { cType = ct', cCont = cc, cScope = cs } <- popCont
  if ct' == ct
    then do
    tell ["returnCont: " ++ show ct ++ " " ++ show value]
    -- scope?
    local (const cs) (cc value)
    else returnCont ct value
