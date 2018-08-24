module Templating.Functions
    ( defaultFunctions
    , callFunc)
    where

import Templating.Types
import Control.Monad.IO.Class (liftIO)
import Data.Ratio (numerator)
import Data.Char (toUpper)

import Control.Monad.Trans.Except

import qualified Data.Map.Strict as M

defaultFunctions :: FunctionStore
defaultFunctions = M.fromList
    [ ("toUpperCase", FuncA1 toUpperCase)
    , ("equal", FuncA2 equal)]

validateArity :: Int -> Int -> Render ()
validateArity actual expected = if actual /= expected
    then throwE $ RenderError $ "Arity error. Expected " ++ (show expected) ++ ", actual: " ++ (show actual) ++ "."
    else return ()

getArity :: Function -> Int
getArity f = case f of {(FuncA0 _) -> 0; (FuncA1 _) -> 1; (FuncA2 _) -> 2; (FuncA3 _) -> 3}

callFunc :: Function -> [Expression] -> Render Expression
callFunc f args = do
    validateArity (length args) (getArity f)
    case f of
        (FuncA0 r) -> r
        (FuncA1 f) -> f (head args)
        (FuncA2 f) -> f (args !! 0) (args !! 1)
        (FuncA3 f) -> f (args !! 0) (args !! 1) (args !! 2)

toUpperCase :: Expression -> Render Expression
toUpperCase (LiteralExpression (LitString str)) = return $ LiteralExpression $ LitString $ map toUpper str
toUpperCase _ = throwE $ FunctionError "toUpperCase: Accepting only strings!"

-- compares literal expressions
equal :: Expression -> Expression -> Render Expression
equal (LiteralExpression l1) (LiteralExpression l2) = return $ LiteralExpression $ LitBool $ l1 == l2
equal _ _ = throwE $ FunctionError "equal: Unable to compare passed!"
