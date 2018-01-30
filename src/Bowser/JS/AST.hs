module Bowser.JS.AST
  ( module Language.JavaScript.Parser.AST
  , JSAst
  , renderJs
  , prettyAst
  ) where

import Language.JavaScript.Parser.AST
import Language.JavaScript.Pretty.Printer

type JSAst = JSAST

renderJs ast = renderToString ast

prettyAst ast = showStripped ast
