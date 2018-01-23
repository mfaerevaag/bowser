module Bowser.DOM.HTML
  ( parseHtml
  , printTree
  ) where

import Text.XML.HXT.Core
import Text.HandsomeSoup hiding (parseHtml)

parseHtml html = readString [withParseHTML yes, withWarnings no] html

printTree tree = do
  res <- runX . xshow $ tree >>> indentDoc
  mapM_ putStrLn res
