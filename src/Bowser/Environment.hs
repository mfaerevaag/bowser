module Bowser.Environment
  ( Env
  , Ident
  , Value (..)
  , emptyEnv
  , lookupEnv
  , insertEnv
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map.Map Ident Value

type Ident = String

data Value = JSUndefined
           | JSNull
           | JSNumber Double
           | JSBoolean Bool
           | JSString String
           -- | JSSymbol -- NOTE: we'll save this for later
           deriving (Eq, Show)

emptyEnv = Map.empty

lookupEnv id env = Map.lookup id env

insertEnv id val env = Map.insert id val env
