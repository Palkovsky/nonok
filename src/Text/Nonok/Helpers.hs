module Text.Nonok.Helpers where

import Text.Nonok.Types
import Text.Nonok.Functions (defaultFunctions, callFunc)

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)

runRenderer :: s -> Renderer w s b a -> IO (Either b a, w)
runRenderer state r = do
    ((result, finalWriter), finalState) <- (runStateT . runWriterT . runExceptT) r state
    return (result, finalWriter)

defaultRenderState :: RenderState
defaultRenderState = RenderState { localVars=M.empty
                                 , globalVars=M.empty
                                 , blocksLookup=M.empty
                                 , scopeStack=[S.empty]
                                 , functions = defaultFunctions}


newRenderState :: VariableLookup -> FunctionStore -> RenderState
newRenderState globals funcs =  defaultRenderState {globalVars = globals, functions = funcs}


noGlobals :: VariableLookup
noGlobals = M.empty

-- \ Check if expression is MapExpression, if so it returns map, otherwise throws error.
getMapFromExpr :: RenderError -> Expression -> Render (M.Map String Expression)
getMapFromExpr err expr = case expr of {(MapExpression m) -> return m; _ -> throwE err}

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
writeString str = lift $ tell $ B.fromString str

writeText :: T.Text -> Render ()
writeText txt = lift $ tell $ B.fromText txt

setVar :: String -> Expression -> Render ()
setVar key lit = do
   state <- getState
   let vars = localVars state
       stack = scopeStack state
   throwIf (null stack) $ RenderError $ "Setting var '" ++ key ++  "' without scope frame present."
   throwIf (M.member key vars) $ RenderError $ "Variable '" ++ key ++ "' already defined."
   let newVars = M.insert key lit vars
       newScope = S.insert key $ head stack
       newStack = newScope:(tail stack)
   putState $ state {localVars=newVars, scopeStack=newStack}

delVar :: String -> Render ()
delVar key = do
   state <- getState
   let vars = localVars state
       stack = scopeStack state
   throwIf (null stack) $ RenderError $ "Deleting variable '$" ++ key ++ "' without scope frame present."
   throwIf (S.notMember key $ head stack) $ RenderError $ "Unable to delete variable '$" ++ key ++ "' outside its scope."
   let newVars = M.delete key vars
       newScope = S.delete key $ head stack
       newStack = newScope:(tail stack)
   putState $ state {localVars=newVars, scopeStack=newStack}

getVar :: String -> Render Expression
getVar key = do
   state <- getState
   let vars = localVars state
   throwNothing (M.lookup key vars) $ RenderError $ "Unable to find variable '$" ++ key ++ "'."

getGlobalVar :: String -> Render Expression
getGlobalVar key = do
   state <- getState
   let vars = globalVars state
   throwNothing (M.lookup key vars) $ RenderError $ "Unable to find variable '@" ++ key ++ "'."


pushFrame :: Render ()
pushFrame = do
    state <- getState
    let stack = scopeStack state
    putState $ state {scopeStack=((S.empty):stack)}

-- |  Pops current frame. Deletes all variables defined in previous scope.
popFrame :: Render ()
popFrame = do
    state <- getState
    let stack = scopeStack state
    throwIf (null stack) $ RenderError "Tried to pop scope frame from empty stack."
    mapM_ delVar $ head stack
    updatedState <- getState --because the state was changed in delVar
    let stackNew = scopeStack updatedState
    putState $ updatedState {scopeStack=(tail stackNew)}

exprToBool :: Expression -> Render Bool
exprToBool (LiteralExpression (LitBool bool)) = return bool
exprToBool (LiteralExpression (LitString str)) = return $ (length str) /= 0
exprToBool (LiteralExpression (LitNum num)) = return $ num /= 0
exprToBool (LiteralExpression (LitRef ref)) = do {expr <- evalLiteral $ LitRef ref; exprToBool expr}
exprToBool e = throwE $ RenderError $ "Unable to evaluate'" ++ (show e) ++ "' to bool."


evalLiteral :: Literal -> Render Expression
evalLiteral (LitRef (RefLocal var)) = do {lit <- getVar var; return lit}
evalLiteral (LitRef (RefGlobal var)) = do {lit <- getGlobalVar var; return lit}
evalLiteral l = return $ LiteralExpression l

evalExpr :: Expression -> Render Expression
evalExpr (LiteralExpression l) = evalLiteral l
evalExpr (ListExpression list) = do {newList <- mapM evalExpr list; return $ ListExpression newList}
evalExpr (MapExpression m) = do {newMap <- mapM evalExpr m; return $ MapExpression newMap}
evalExpr (FuncExpression name args) = do
    state <- getState
    f <- throwNothing (M.lookup name (functions state)) $ RenderError $ "Function '" ++ name ++ "' doesn't exists."
    resolvedArgs <- mapM evalExpr args --arguments should be resolved to basic form
    result <- callFunc f resolvedArgs -- probably need to add executing and error handling here
    evalExpr result
evalExpr (MapMemberExpression ref keys) = do
    lMap <- evalLiteral $ LitRef ref
    parseNext lMap keys
    where
        parseNext m [] = return m
        parseNext lMap [key] = do
            case lMap of
                (MapExpression m) -> do
                   content <- throwNothing (M.lookup key m) $ RenderError $ "Tried to access unexistent map member '" ++ key ++ "'."
                   return content
                _ -> throwE $ RenderError $ "Tried to access field '" ++ key ++ "' of non-map structure."
        parseNext lMap (key:rest) = do
            case lMap of
                (MapExpression m) -> do
                    content <- throwNothing (M.lookup key m) $ RenderError $ "Tried to access unexistent map member '" ++ key ++ "'."
                    parseNext content rest
                _ -> throwE $ RenderError $ "Tried to access field '" ++ key ++ "' of non-map structure."
