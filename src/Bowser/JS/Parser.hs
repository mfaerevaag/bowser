module Bowser.JS.Parser
  ( parseJs
  , printJs
  ) where

import Language.JavaScript.Parser.Parser (readJs)
import Language.JavaScript.Pretty.Printer (renderToString)

parseJs js = readJs js

printJs ast = putStrLn $ renderToString ast
