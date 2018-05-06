module Bowser.Types
  ( Ident
  , Scope
  , Value (..)
  , NativeObj (..)
  , emptyObject
  , newObject
  , lookupObject
  , insertObject
  , newFunc
  , valueToBool
  , extractTaint
  ) where

import Data.Maybe
import Data.Tainted
import Data.Map (Map)
import qualified Data.Map as Map
import Bowser.AST

type Ident = String

type Scope = Value

data Value = JSUndefined
           | JSNull
           | JSNumber (Tainted Double)
           | JSBoolean (Tainted Bool)
           | JSString (Tainted String)
           | JSObject { tab :: Map.Map Ident Value
                      , native :: NativeObj
                      }
           deriving Eq

data NativeObj = SimpleObj
               | FuncObj { name :: Maybe String
                         , params :: [Ident]
                         , code  :: JSStatement
                         }
               deriving Eq

-- object

emptyObject = JSObject Map.empty SimpleObj

newObject :: [(Ident, Value)] -> Value
newObject tab = JSObject { tab = (Map.fromList tab), native = SimpleObj }

lookupObject val id = case val of
  JSObject { tab = tab } -> Map.lookup id tab
  JSString s -> case id of
    "length" -> Just . JSNumber . Clean . fromIntegral . length . extractTaint $ s
    _ -> Just JSUndefined
  _ -> Just JSUndefined

insertObject (JSObject { tab = tab }) id val = JSObject { tab = (Map.insert id val tab)
                                                       , native = SimpleObj }

newFunc :: Maybe String -> [Ident]-> JSStatement -> Value
newFunc name params block = JSObject { tab = Map.empty
                                     , native = FuncObj { name = name
                                                        , params = params
                                                        , code = block } }

-- other

valueToBool :: Value -> Bool
valueToBool val = case val of
  JSNumber (Clean n) -> n /= 0.0
  JSNumber (Dirty n) -> n /= 0.0
  JSString (Clean s) -> (length s) > 0
  JSString (Dirty s) -> (length s) > 0
  JSBoolean (Clean False) -> False
  JSBoolean (Dirty False) -> False
  JSUndefined -> False
  JSNull -> False
  _ -> True

instance Show Value where
  show JSUndefined = "undefined"
  show JSNull = "null"
  show (JSNumber n) = show n
  show (JSBoolean b) = show b
  show (JSString s) = show s
  show (JSObject tab native) = case native of
    SimpleObj -> "{ " ++ (drop 2 (foldr (\(key, val) acc ->
                                            ", " ++ key ++ ": " ++ (show val) ++ acc
                                        ) "" (Map.toList tab))) ++ " }"
    FuncObj name _ _ -> "[Function: " ++ (fromMaybe "anonymous" name) ++ "]"

extractTaint :: Tainted a -> a
extractTaint (Clean a) = a
extractTaint (Dirty a) = a
