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

type State = (Integer, Maybe Integer)

newState :: Maybe Integer -> State
newState threshold = (0, threshold)

incState :: Engine ()
incState = do
  (st, threshold) <- get
  case threshold of
    Just t -> if (st > t)
      then throwError "eval stopped: step threshold reached"
      else put ((st + 1), threshold)
    Nothing -> put ((st + 1), threshold)
