module Bowser.Parser
  ( parseFile
  , parseString
  ) where

import Language.JavaScript.Parser.Parser

parseString = readJs
