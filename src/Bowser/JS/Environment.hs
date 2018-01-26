module Bowser.JS.Environment
  ( JSEnv
  , JSValue (..)
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

data JSEnv = Map String JSValue

-- TODO: add all types
data JSValue = JSInt Int
             | JSString String
             deriving (Show)
