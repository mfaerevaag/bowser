module Bowser.JS.AST
  ( module Language.JavaScript.Parser.AST
  , JSAst
  , printJs
  , printAst
  ) where

import Language.JavaScript.Parser.AST
import Language.JavaScript.Pretty.Printer

type JSAst = JSAST

printJs ast = putStrLn $ renderToString ast

printAst ast = putStrLn $ showStripped ast
