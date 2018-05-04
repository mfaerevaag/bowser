module Bowser.Helper
  ( strip
  , trim
  , consumeCommaList
  , consumeCommaTrailingList
  ) where

import Data.Char (isSpace)

import Bowser.AST (JSCommaList(..), JSCommaTrailingList(..))

-- string helpers

-- |Remove quotes from ends of string
strip :: String -> String
strip = f . f
  where f = reverse . dropWhile (`elem` "'\"")

-- |Remove whitespace from ends of string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- ast helpers

consumeCommaList :: (JSCommaList a) -> [a]
consumeCommaList clist = case clist of
  (JSLCons xs _ x) -> x:(consumeCommaList xs)
  (JSLOne x) -> [x]
  (JSLNil) -> []

consumeCommaTrailingList :: (JSCommaTrailingList a) -> [a]
consumeCommaTrailingList clist = case clist of
  (JSCTLComma xs _) -> consumeCommaList xs
  (JSCTLNone xs) -> consumeCommaList xs
