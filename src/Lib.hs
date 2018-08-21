module Lib
    ( module Lib
    ) where

import Templating.Parser as Lib
import Templating.Types as Lib
import Templating.RenderHelpers as Lib
import Templating.Render as Lib

entrypoint :: IO ()
entrypoint = putStrLn "someFunc"

feed :: String -> IO ()
feed str = do
    case parseAll <^> str of
        Left err  -> putStrLn $ show err
        Right ast -> do
            (e, rendered) <- runRenderer initialRenderState $ render ast
            putStrLn rendered
