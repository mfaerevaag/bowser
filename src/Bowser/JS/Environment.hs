module Bowser.JS.Environment
  ( Env
  , Ident
  , Value (..)
  , emptyEnv
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map.Map Ident Value

type Ident = String

-- TODO: implement all types
data Value = JSInt Integer
           | JSString String
           deriving (Show)

emptyEnv = Map.empty
