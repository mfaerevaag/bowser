module Bowser.GlobalObject where

import Bowser.Types

newGlobalObject = newObject [ ("undefined", JSUndefined)
                            , ("document", newObject [ ("cookie", JSString "") ])
                            ]
