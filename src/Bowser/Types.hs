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
  , valueToBool
  ) where

import Data.Maybe
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
           deriving (Eq)

data NativeObj = SimpleObj
               | FuncObj { name :: Maybe String
                         , params :: [Ident]
                         , code  :: [JSStatement]
                         }
               deriving (Eq)

-- scope

emptyScope = Map.empty

lookupScope scope id = Map.lookup id scope

insertScope scope id val = Map.insert id val scope

-- object

emptyObject = JSObject Map.empty

newObject :: [(Ident, Value)] -> Value
newObject props = JSObject { props = (Map.fromList props), native = SimpleObj }

newFunc :: Maybe String -> [Ident]-> [JSStatement] -> Value
newFunc name params stmts = JSObject { props = emptyScope
                                     , native = FuncObj { name = name
                                                        , params = params
                                                        , code = stmts } }

-- type helper

valueToBool :: Value -> Bool
valueToBool val = case val of
  JSBoolean False -> False
  JSUndefined -> False
  JSNull -> False
  _ -> True

instance Show Value where
  show JSUndefined = "undefined"
  show JSNull = "null"
  show (JSNumber n) = show n
  show (JSBoolean b) = show b
  show (JSString s) = s
  show (JSObject props native) = case native of
    SimpleObj -> "{ " ++ (drop 2 (foldr (\(key, val) acc ->
                                            ", " ++ key ++ ": " ++ (show val) ++ acc
                                        ) "" (Map.toList props))) ++ " }"
    FuncObj name _ _ -> "[Function: " ++ (fromMaybe "anonymous" name) ++ "]"
