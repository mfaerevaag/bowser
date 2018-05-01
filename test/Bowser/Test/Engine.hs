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
    testCase "Binary expr add" $ wrap (JSNumber 3.0) "1 + 2"
  , testCase "Binary expr subtract" $ wrap (JSNumber (-1.0)) "1 - 2"
  , testCase "Binary expr concat" $ wrap (JSString "foobar") "'foo' + 'bar'"
  , testCase "Binary expr concat cast 1" $ wrap (JSString "1.0bar") "1 + 'bar'"
  , testCase "Binary expr concat cast 2" $ wrap (JSString "foo2.0") "'foo' + 2"
  ]
