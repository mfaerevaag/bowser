module Main where

import Text.Show.Pretty

import Bowser.JS.Parser
import Bowser.JS.Engine
import Bowser.JS.AST

main :: IO ()
main = do
  ast <- parseJs path

  putStrLn "ast:"
  pPrint ast
  putStrLn ""

  putStrLn "return:"
  print $ runJs ast

  where
    path = "examples/foo.js"
