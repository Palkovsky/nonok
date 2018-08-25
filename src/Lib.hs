module Lib
    ( module Lib
    ) where

import Templating.Parser as Lib
import Templating.Types as Lib
import Templating.Expressible as Lib
import Templating.Helpers as Lib
import Templating.Render as Lib

entrypoint :: IO ()
entrypoint = putStrLn "someFunc"
