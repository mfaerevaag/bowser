module Main where

import Bowser.JS.Parser
import Bowser.JS.Engine
import Bowser.JS.AST

main :: IO ()
main = do
  ast <- parseJs path

  putStrLn "ast:"
  printAst ast
  putStrLn ""

  putStrLn "return:"
  print $ runJs ast

  where
    path = "examples/foo.js"
