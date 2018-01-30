module Bowser.JS.Environment
  ( JSEnv
  , JSValue (..)
  ) where

import qualified Data.Map as Map

data JSEnv = Map String JSValue

-- TODO: add all types
data JSValue = JSInt Integer
             | JSString String
             deriving (Show)
