module Text.Nonok.Functions
    ( defaultFunctions
    , callFunc
    , newFunc)
    where

import Text.Nonok.Types
import Text.Nonok.Expressible
import Control.Monad.IO.Class (liftIO)
import Data.Ratio (numerator)

import Data.List (intersperse)
import Data.Char (toUpper, toLower)

import Control.Monad.Trans.Except
import Data.Foldable (foldrM)

import qualified Data.Text as T
import qualified Data.Map.Strict as M

defaultFunctions :: FunctionStore
defaultFunctions = M.fromList
    [ ("upper", FuncA1 toUpperF)
    , ("lower", FuncA1 toLowerF)

    , ("equal", FuncA2 equalF)
    , ("gt", FuncA2 gtF)
    , ("gte", FuncA2 gteF)
    , ("lt", FuncA2 ltF)
    , ("lte", FuncA2 lteF)

    , ("or", FuncA2 orF)
    , ("and", FuncA2 andF)

    , ("strip", FuncA1 stripF)
    , ("escape", FuncA1 escapeF)
    , ("replace", FuncA3 replaceF)
    , ("concat", FuncA2 concatF)
    , ("concat_arr", FuncA1 concatArrF)
    , ("intersperse", FuncA2 intersperseF)]

newFunc :: String -> Function -> FunctionStore -> FunctionStore
newFunc = M.insert

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


extractString :: Expression -> RenderError -> Render String
extractString expr err =
    case expr of {LiteralExpression (LitString str) -> return str; _ -> throwE err}

extractList :: Expression -> RenderError -> Render [Expression]
extractList expr err =
    case expr of {ListExpression exprs -> return exprs; _ -> throwE err}

-- Couple of standard functions below

toUpperF :: Expression -> Render Expression
toUpperF expr = do
    str <- extractString expr $ RenderError "upper: Accepting only strings!"
    return $ express $ map toUpper str

toLowerF :: Expression -> Render Expression
toLowerF expr = do
    str <- extractString expr $ RenderError "lower: Accepting only strings!"
    return $ express $ map toLower str

stripF :: Expression -> Render Expression
stripF expr = do
    str <- extractString expr $ RenderError "strip: Accepting only strings!"
    (return . express . T.unpack . T.strip . T.pack) str

replaceF :: Expression -> Expression -> Expression -> Render Expression
replaceF e1 e2 e3 = do
    needle <- extractString e1 $ RenderError "replace: First arg must be a string!"
    replacement  <- extractString e2 $ RenderError "replace: Second arg must be a string!"
    haystack  <- extractString e3 $ RenderError "replace: Third arg must be a string!"
    return $ express $ T.unpack $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)

concatF :: Expression -> Expression -> Render Expression
concatF e1 e2 = do
    str1 <- extractString e1 $ RenderError "concat: First arg must be a string!"
    str2  <- extractString e2 $ RenderError "concat: Second arg must be a string!"
    return $ express $ str1 ++ str2

concatArrF :: Expression -> Render Expression
concatArrF e = do
    list <- extractList e $ RenderError "concat_arr: Accepting only string lists!"
    foldrM concatF (express "") list

orF :: Expression -> Expression -> Render Expression
orF e1 e2 = do {b1 <- litToBool e1; b2 <- litToBool e2; return $ express $ b1 || b2}

andF :: Expression -> Expression -> Render Expression
andF e1 e2 = do {b1 <- litToBool e1; b2 <- litToBool e2; return $ express $ b1 && b2}

intersperseF :: Expression -> Expression -> Render Expression
intersperseF (LiteralExpression (LitString split)) (LiteralExpression (LitString str)) = do
    let charArr = map (\char -> [char])  str
    return $ express $ foldr (++) "" $ intersperse split charArr
intersperseF _ _ = throwE $ RenderError "intersperse: accepting only split and string"

-- escapes html characters
escapeF :: Expression -> Render Expression
escapeF (LiteralExpression (LitString str)) = do
    let replacements = [(">", "&gt;"), ("<", "&lt;"), ("&", "&amp;")]
    return $ express $ foldr (\(needle, replacement) acc -> T.replace (T.pack needle) (T.pack replacement) acc) (T.pack str) replacements
escapeF _ = throwE $ RenderError "escape: expected string"

-- compares literal expressions
equalF :: Expression -> Expression -> Render Expression
equalF (LiteralExpression l1) (LiteralExpression l2) = return $ express $ l1 == l2
equalF _ _ = throwE $ RenderError "Uncomparable"

compF :: (Integer -> Integer -> Bool) -> Expression -> Expression -> Render Expression
compF f (LiteralExpression (LitNum l1)) (LiteralExpression (LitNum l2)) = return $ express $ f l1 l2
compF f (LiteralExpression (LitString l1)) (LiteralExpression (LitString l2)) = return $ express $ ((fromIntegral . length) l1) `f` ((fromIntegral . length) l2)
compF f (ListExpression exprs1) (ListExpression exprs2) = return $ express $ ((fromIntegral . length) exprs1) `f` ((fromIntegral . length) exprs2)
compF _ _ _ = throwE $ RenderError "Uncomparable"

gtF = compF (>)
gteF = compF (>=)
ltF = compF (<)
lteF = compF (<=)

litToBool :: Expression -> Render Bool
litToBool (LiteralExpression (LitBool bool)) = return bool
litToBool (LiteralExpression (LitString str)) = return $ (length str) /= 0
litToBool (LiteralExpression (LitNum num)) = return $ num /= 0
litToBool (ListExpression list) = return $ (length list) /= 0
litToBool e = throwE $ RenderError $ "Unable to evaluate'" ++ (show e) ++ "' to bool."
