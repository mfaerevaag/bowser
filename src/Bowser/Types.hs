module Bowser.Types
  ( Ident
  , Value (..)
  , Scope
  , emptyScope
  , lookupScope
  , insertScope
  , emptyObject
  , newObject
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
           | JSObject { props :: Scope
                              -- TODO
                      }
           -- | JSSymbol -- NOTE: we'll save this for later
           deriving (Eq, Show)

-- scope

emptyScope = Map.empty

lookupScope id env = Map.lookup id env

insertScope id val env = Map.insert id val env

-- object

emptyObject = JSObject Map.empty

newObject :: [(Ident, Value)] -> Value
newObject props = JSObject { props = (Map.fromList props) }
