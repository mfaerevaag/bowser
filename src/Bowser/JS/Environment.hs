module Bowser.JS.Environment
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
           deriving (Show)

emptyEnv = Map.empty

lookupEnv id env = Map.lookup id env

insertEnv id val env = Map.insert id val env
