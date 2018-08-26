module Text.Nonok.Render where

import Text.Nonok.Types
import Text.Nonok.Parser (generateAST)
import Text.Nonok.Helpers

import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class (liftIO)

import System.Directory
import System.FilePath (takeDirectory)

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)


feed :: VariableLookup -> String -> IO (Either String String)
feed globals str =
    case generateAST str of
        Left err  -> return $ Left $ show err
        Right ast -> do
            (e, rendered) <- runRenderer (initialRenderState globals) $ render ast
            return $ case e of {Left err -> Left $ show err; Right _ -> Right rendered}

feedFromFile :: VariableLookup -> FilePath -> IO (Either String String)
feedFromFile globals path = do
    exists <- doesFileExist path
    if exists
    then do
        curDir <- getCurrentDirectory
        contents <- readFile path
        setCurrentDirectory $ takeDirectory path
        result <- feed globals contents
        setCurrentDirectory curDir
        return result
    else return $ Left $ "Unable to resolve path '" ++ path ++ "'."


render :: [Piece] -> Render ()
render (piece:xs) = do
    case piece of
        (StaticPiece str) -> writeString str
        (CommentPiece) -> return ()
        (RawPiece str) -> writeString str
        (IncludeRefPiece ref globals) -> renderIncludeRef ref globals
        (IncludePathPiece path globals) -> renderIncludePath path globals
        (ForPiece var expr pieces) -> renderFor var expr pieces
        (IfPiece exprs piecesList) -> renderIf exprs piecesList
        (CallPiece expr) -> renderCall expr
        (Decl decs) -> renderDecl decs
    render xs

render [] = return ()


renderFor :: String -> Expression -> [Piece] -> Render ()
renderFor var (LiteralExpression (LitString iterable)) pieces =
    mapM_ (\char -> do
        pushFrame
        setVar var $ LiteralExpression $ LitString [char]
        render pieces
        popFrame) iterable

renderFor var (LiteralExpression (LitRef ref)) pieces = do
    contents <- evalLiteral $ LitRef ref
    renderFor var contents pieces

renderFor var (ListExpression exprs) pieces =
    mapM_ (\expr -> do
        pushFrame
        lit <- evalExpr expr
        setVar var lit
        render pieces
        popFrame) exprs

renderFor var (FuncExpression name args) pieces = do
    expr <- evalExpr $ FuncExpression name args
    renderFor var expr pieces

renderFor var _ pieces = throwE $ RenderError "not implemented yet"

renderDecl :: [(String, Expression)] -> Render ()
renderDecl ((var, expr):xs) = do
    lit <- evalExpr expr
    setVar var lit
    renderDecl xs
renderDecl [] = return ()

renderCall :: Expression -> Render ()
renderCall expr = do
   evaluated <- evalExpr expr
   writeString $ show $ PrintableExpression evaluated

renderIf :: [Expression] -> [[Piece]] -> Render ()
renderIf (expr:xs) (pieces:ys) = do
    literal <- evalExpr expr
    boolean <- exprToBool literal
    if boolean then do
        pushFrame
        render pieces
        popFrame
    else renderIf xs ys
renderIf [] [] = return ()
renderIf _ _ = throwE $ RenderError "Unable to match expressions with blocks."

mergedGlobals :: Maybe Expression -> Render (M.Map String Expression)
mergedGlobals maybeMapExpr = do
    state <- getState
    globalsExpr <- maybe (return $ MapExpression M.empty) evalExpr maybeMapExpr
    globalsMap <- getMapFromExpr (RenderError "Not map passed as new globals.") globalsExpr
    return $ merge preserveMissing preserveMissing (zipWithMatched (\_ _ x ->  x)) (globalVars state) globalsMap

renderIncludeRef :: Reference -> Maybe Expression -> Render ()
renderIncludeRef ref maybeMapExpr = do
    newGlobals <- mergedGlobals maybeMapExpr
    expr <- evalLiteral $ LitRef ref
    let str = show $ PrintableExpression expr
    state <- getState
    result <- liftIO $ feed newGlobals str
    case result of
        (Left err) -> throwE $ RenderError err
        (Right rendered) -> writeString rendered

renderIncludePath :: String -> Maybe Expression -> Render ()
renderIncludePath path maybeMapExpr = do
    newGlobals <- mergedGlobals maybeMapExpr
    state <- getState
    result <- liftIO $ feedFromFile newGlobals path
    case result of
        (Left err) -> throwE $ RenderError err
        (Right rendered) -> writeString rendered
