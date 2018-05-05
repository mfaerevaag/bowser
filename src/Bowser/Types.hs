module Bowser.Types
  ( Ident
  , Scope
  , Value (..)
  , NativeObj (..)
  , emptyScope
  , lookupScope
  , insertScope
  , emptyObject
  , newObject
  , newFunc
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Bowser.AST

type Ident = String

type Scope = Map.Map Ident Value

data Value = JSUndefined
           | JSNull
           | JSNumber Double
           | JSBoolean Bool
           | JSString String
           | JSObject { props :: Scope
                      , native :: NativeObj
                      }
           -- | JSSymbol -- NOTE: we'll save this for later
           deriving (Show, Eq)

data NativeObj = SimpleObj
               | FuncObj { params :: [Ident]
                         , code  :: [JSStatement]
                         }
               deriving (Show, Eq)

-- scope

emptyScope = Map.empty

lookupScope scope id = Map.lookup id scope

insertScope scope id val = Map.insert id val scope

-- object

emptyObject = JSObject Map.empty

newObject :: [(Ident, Value)] -> Value
newObject props = JSObject { props = (Map.fromList props), native = SimpleObj }

newFunc :: [Ident] -> [JSStatement] -> Value
newFunc params stmts = JSObject { props = emptyScope, native = FuncObj { params = params, code = stmts } }
