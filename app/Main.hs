module Main where

import Bowser.DOM.HTML

main :: IO ()
main = do
  html <- readFile path
  printTree $ parseHtml html
  where
    path = "examples/foo.html"
