module Bowser.Helper
  ( strip
  , trim
  ) where

import Data.Char (isSpace)

-- |Remove quotes from ends of string
strip :: String -> String
strip = f . f
  where f = reverse . dropWhile (`elem` "'\"")

-- |Remove whitespace from ends of string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
