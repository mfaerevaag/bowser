module Bowser.Test.Engine (engineTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Bowser.AST
import Bowser.Environment
import Bowser.Parser
import Bowser.Engine

-- wrap :: String -> Either String Value
wrap e s = do
  ((x, _), _) <- eval (parseString s)
  case x of
    Left e -> fail e
    Right x -> x @?= e


engineTests = testGroup "Engine"
  [
    testCase "Binary expr" $ wrap (JSNumber 3.0) "1 + 2"
  , testCase "Binary expr" $ wrap (JSNumber (-1.0)) "1 - 2"
  ]
