module Bowser.Helper
  (strip) where

strip = f . f
  where f = reverse . dropWhile (`elem` "'\"")
