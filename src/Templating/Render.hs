module Templating.Render where

import Templating.Types
import Templating.Parser (generateAST)
import Templating.RenderHelpers

import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class (liftIO)

import System.Directory

feed :: VariableLookup -> String -> IO (Either String String)
feed globals str =
    case generateAST str of
        Left err  -> return $ Left $ show err
        Right ast -> do
            (e, rendered) <- runRenderer (initialRenderState globals) $ render ast
            return $ case e of {Left err -> Left $ show err; Right _ -> Right rendered}

render :: [Piece] -> Render ()
render (piece:xs) = do
    case piece of
        (StaticPiece str) -> writeString str
        (ForPiece var expr pieces) -> renderFor var expr pieces
        (IfPiece exprs piecesList) -> renderIf exprs piecesList
        (CallPiece expr) -> renderCall expr
        (Decl decs) -> renderDecl decs
        (IncludeRefPiece ref) -> renderIncludeRef ref
        (IncludePathPiece path) -> renderIncludePath path
        _  -> return ()
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

renderIncludeRef :: Reference -> Render ()
renderIncludeRef ref = do
    expr <- evalLiteral $ LitRef ref
    let str = show $ PrintableExpression expr
    state <- getState
    result <- liftIO $ feed (globalVars state) str
    case result of
        (Left err) -> throwE $ RenderError err
        (Right rendered) -> writeString rendered

renderIncludePath :: String -> Render ()
renderIncludePath path = do
    exists <- liftIO $ doesFileExist path
    throwUnless exists $ RenderError "Unexistent file specified in include."
    contents <- liftIO $ readFile path
    state <- getState
    result <- liftIO $ feed (globalVars state) contents
    case result of
        (Left err) -> throwE $ RenderError err
        (Right rendered) -> writeString rendered
