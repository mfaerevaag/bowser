module Main where

-- import Bowser.DOM.HTML
import Bowser.JS.Parser
import Bowser.JS.Engine

-- main :: IO ()
-- main = do
--   html <- readFile path
--   printTree $ parseHtml html
--   where
--     path = "examples/foo.html"

main :: IO ()
main = do
  js <- readFile path

  putStrLn "ast:"
  print $ parseJs js
  putStrLn ""

  putStrLn "return:"
  print $ runJs $ parseJs js

  where
    path = "examples/foo.js"
