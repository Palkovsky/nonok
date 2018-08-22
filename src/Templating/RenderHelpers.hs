module Templating.RenderHelpers where

import Templating.Types

import qualified Data.Map.Strict as M

import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)

runRenderer :: s -> Renderer w s b a -> (Either b a, w)
runRenderer state r =
    let
        identity = (runStateT . runWriterT . runExceptT) r state
        ((result, finalWriter), finalState) = runIdentity identity
    in (result, finalWriter)


initialRenderState :: RenderState
initialRenderState = M.empty

getState :: Render RenderState
getState = lift $ lift get

putState :: RenderState -> Render ()
putState r = lift $ lift $ put r

throwMaybe :: Maybe a -> RenderError -> Render a
throwMaybe m err =
    case m of
        Just x  -> return x
        Nothing -> throwE err

writeString :: String -> Render ()
writeString str = lift $ tell str

setVar :: String -> Literal -> Render ()
setVar key lit = do
   state <- getState
   let newState = M.insert key lit state
   putState newState

delVar :: String -> Render ()
delVar key = do
   state <- getState
   let newState = M.delete key state
   putState newState

getVar :: String -> Render Literal
getVar key = do
   state <- getState
   throwMaybe (M.lookup key state) (RenderError $ "Unable to find variable '" ++ key ++ "'.")

literalToBool :: Literal -> Render Bool
literalToBool (LitBool bool) = return bool
literalToBool (LitString str) = return $ (length str) /= 0
literalToBool (LitInteger int) = return $ int /= 0
literalToBool (LitDouble double) = return $ double /= 0
literalToBool _ = throwE $ RenderError "Unable to evaluate literal to bool."

evalExpr :: Expression -> Render Literal
evalExpr (LiteralExpression lit) = return lit
evalExpr (ReferenceExpression var) = do {lit <- getVar var; return lit}
evalExpr (ListExpression list) = do {literals <- mapM evalExpr list; return $ LitList literals}
