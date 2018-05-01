module Bowser.Test.Helper (t) where

import Test.Tasty
import Test.Tasty.HUnit

import Bowser.Parser
import Bowser.Engine

t e s = do
  ((x, _), _) <- eval (parseString s)
  case x of
    Left e -> fail e
    Right x -> x @?= e
