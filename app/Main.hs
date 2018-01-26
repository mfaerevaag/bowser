module Main where

-- import Bowser.DOM.HTML
import Bowser.JS.Parser

-- main :: IO ()
-- main = do
--   html <- readFile path
--   printTree $ parseHtml html
--   where
--     path = "examples/foo.html"

main :: IO ()
main = do
  js <- readFile path
  printJs $ parseJs js
  where
    path = "examples/foo.js"
