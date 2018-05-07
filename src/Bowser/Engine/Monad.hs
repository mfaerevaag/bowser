module Bowser.Engine.Monad where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer
import Control.Monad.Cont

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
                   , threshold :: Maybe Integer }
           deriving Show

newState :: Maybe Integer -> State
newState threshold = State 0 threshold

incState :: Engine ()
incState = do
  State { step = st, threshold = th } <- get
  -- check step counter
  case th of
    Just t -> when (st > t) $
      throwError "eval stopped: step threshold reached"
    _ -> return ()
  put $ State { step = (st + 1), threshold = th }
