module Bowser.Test.Helper (t) where

import Test.Tasty
import Test.Tasty.HUnit

import Bowser.Parser
import Bowser.Engine.Interp

t e s = do
  (x, _) <- eval (parseString s) (Just 1000)
  case x of
    Left e -> fail e
    Right x -> x @?= e
