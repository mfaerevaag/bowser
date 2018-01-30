module Main where

import Text.Show.Pretty

import Bowser.JS.Parser
import Bowser.JS.Engine
import Bowser.JS.AST

main :: IO ()
main = do
  putStrLn "ast:"
  ast <- parseJs path
  pPrint ast

  putStrLn ""

  putStrLn "return:"
  res <- runJs ast
  pPrint res

  where
    path = "examples/foo.js"
