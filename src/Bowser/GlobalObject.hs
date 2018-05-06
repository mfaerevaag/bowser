module Bowser.GlobalObject where

import Data.Tainted
import Bowser.Types

newGlobalObject = newObject [ ("undefined", JSUndefined)
                            , ("document", newObject [ ("cookie", JSString (Dirty "secret")) ])
                            ]
