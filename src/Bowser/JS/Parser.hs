module Bowser.JS.Parser
  ( parseJs
  , printJs
  ) where

import Language.JavaScript.Parser.Parser
import Language.JavaScript.Pretty.Printer (renderToString)

parseJs = parseFile

printJs ast = putStrLn $ renderToString ast
