module Bowser.JS.Engine
  ( runJs
  ) where

import Bowser.JS.AST
import Bowser.JS.Environment

runJs :: JSAst -> Either String JSValue
runJs ast =
  case ast of
    JSAstProgram (stmt:_) _ -> runStmt stmt
    otherwise -> Left "not implemented yet"

runStmt :: JSStatement -> Either String JSValue
runStmt stmt = Right (JSInt 42) -- TODO: implement
