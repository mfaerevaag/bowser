module Bowser.Types
  ( Ident
  , Value (..)
  , Scope
  , emptyScope
  , lookupScope
  , insertScope
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

type Ident = String

type Scope = Map.Map Ident Value

data Value = JSUndefined
           | JSNull
           | JSNumber Double
           | JSBoolean Bool
           | JSString String
           -- | JSSymbol -- NOTE: we'll save this for later
           deriving (Eq, Show)

emptyScope = Map.empty

lookupScope id env = Map.lookup id env

insertScope id val env = Map.insert id val env
