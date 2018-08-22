module Templating.RenderHelpers where

import Templating.Types

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)

runRenderer :: s -> Renderer w s b a -> (Either b a, w)
runRenderer state r =
    let identity = (runStateT . runWriterT . runExceptT) r state
        ((result, finalWriter), finalState) = runIdentity identity
    in (result, finalWriter)


initialRenderState :: RenderState
initialRenderState = (M.empty, [S.empty])

getState :: Render RenderState
getState = lift $ lift get

putState :: RenderState -> Render ()
putState r = lift $ lift $ put r

throwNothing :: Maybe a -> RenderError -> Render a
throwNothing m err =
    case m of
        Just x  -> return x
        Nothing -> throwE err

throwJust :: Maybe a -> RenderError -> Render ()
throwJust m err =
    case m of
        Just _  -> throwE err
        Nothing -> return ()

throwIf :: Bool -> RenderError -> Render ()
throwIf b err = if b then throwE err else return ()

throwUnless :: Bool -> RenderError -> Render ()
throwUnless b err = if b then return () else throwE err

writeString :: String -> Render ()
writeString str = lift $ tell str

setVar :: String -> Literal -> Render ()
setVar key lit = do
   (vars, stack) <- getState
   throwIf (null stack) $ RenderError "Setting var without scope frame."
   throwIf (M.member key vars) $ RenderError $ "Variable '" ++ key ++ "' already defined."
   let newVars = M.insert key lit vars
   let newScope = S.insert key $ head stack
   putState (newVars, newScope:(tail stack))

delVar :: String -> Render ()
delVar key = do
   (vars, stack) <- getState
   throwIf (null stack) $ RenderError "Deleting var without scope frame."
   throwIf (S.notMember key $ head stack) $ RenderError "Trying to delete variable outside current scope frame."
   let newVars = M.delete key vars
   let newScope = S.delete key $ head stack
   putState (newVars, newScope:(tail stack))

getVar :: String -> Render Literal
getVar key = do
   (vars, _) <- getState
   throwNothing (M.lookup key vars) (RenderError $ "Unable to find variable '" ++ key ++ "'.")

pushFrame :: Render ()
pushFrame = do
    (vars, stack) <- getState
    putState (vars, [S.empty] ++ stack)

-- |  Pops current frame. Deletes all variables defined in previous scope.
popFrame :: Render ()
popFrame = do
    (_, stack) <- getState
    throwIf (null stack) $ RenderError "Tried to pop scope frame from empty stack."
    mapM_ delVar $ head stack
    (newVars, _) <- getState --because it was edited by delVar
    putState (newVars, tail stack)

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
