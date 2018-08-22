module Lib
    ( module Lib
    ) where

import Templating.Parser as Lib
import Templating.Types as Lib
import Templating.RenderHelpers as Lib
import Templating.Render as Lib

entrypoint :: IO ()
entrypoint = putStrLn "someFunc"

feed :: String -> Either String String
feed str =
    case parseAll <^> str of
        Left err  -> Left $ show err
        Right ast -> do
            let (e, rendered) = runRenderer initialRenderState $ render ast
            case e of {Left err -> Left $ show err; Right _ -> Right rendered}
