module Main where

import Text.Show.Pretty

import Bowser.Parser
import Bowser.Engine
import Bowser.AST

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
