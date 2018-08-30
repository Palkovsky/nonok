module Text.Nonok.Render where

import Text.Nonok.Types
import Text.Nonok.Parser (generateAST)
import Text.Nonok.Helpers
import Text.Nonok.Functions (defaultFunctions)


import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class (liftIO)

import System.Directory
import System.FilePath (takeDirectory)

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)


feed :: RenderState -> T.Text -> IO (Either RenderError T.Text)
feed state text =
    case generateAST (T.unpack text) of
        Left err  -> return $ Left $ RenderError $ show err
        Right ast -> do
            (e, builder) <- runRenderer state $ render ast
            return $
              case e of
                  (Left (RenderError err)) ->  Left $ RenderError err
                  (Left (ParsingError err)) -> Left $ RenderError $ show err
                  _ -> Right $ LT.toStrict $ B.toLazyText builder

feedFromFile :: RenderState -> FilePath -> IO (Either RenderError T.Text)
feedFromFile state path = do
    exists <- doesFileExist path
    if exists
    then do
        curDir <- getCurrentDirectory
        contents <- TIO.readFile path
        -- this is not thread-safe, that's way some tests might fail
        setCurrentDirectory $ takeDirectory path
        result <- feed state contents
        setCurrentDirectory curDir
        case result of
            (Left (RenderError err)) -> return $ Left $ RenderError $ path ++ " -> " ++ err
            (Left (ParsingError err)) -> return $ Left $ RenderError $ path ++ " -> " ++ (show err)
            _ -> return result
    else return $ Left $ RenderError $ "Unable to resolve path '" ++ path ++ "'."


render :: [Piece] -> Render ()
render ((ExtendsPiece path):xs) = renderExtends path xs --if it's extended. give render control to parent
render (piece:xs) = do
    case piece of
        (ExtendsPiece path) -> return ()
        (BlockPiece identifier pieces) -> renderBlock identifier pieces
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

renderFor var _ pieces = throwE $ RenderError "Uniterable expression in for."

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
renderIf _ _ = throwE $ RenderError "Unable to evaluate if blocks."

mergedGlobals :: Maybe Expression -> Render VariableLookup
mergedGlobals maybeMapExpr = do
    state <- getState
    globalsExpr <- maybe (return $ MapExpression M.empty) evalExpr maybeMapExpr
    globalsMap <- getMapFromExpr (RenderError "Not map passed as new globals.") globalsExpr
    return $ merge preserveMissing preserveMissing (zipWithMatched (\_ _ x ->  x)) (globalVars state) globalsMap

renderIncludeRef :: Reference -> Maybe Expression -> Render ()
renderIncludeRef ref maybeMapExpr = do
    newGlobals <- mergedGlobals maybeMapExpr
    expr <- evalLiteral $ LitRef ref
    let str = T.pack $ show $ PrintableExpression expr
    result <- liftIO $ feed (newRenderState newGlobals defaultFunctions) str
    case result of
        (Left err) -> throwE err
        (Right rendered) -> writeText rendered

renderIncludePath :: String -> Maybe Expression -> Render ()
renderIncludePath path maybeMapExpr = do
    newGlobals <- mergedGlobals maybeMapExpr
    result <- liftIO $ feedFromFile (newRenderState newGlobals defaultFunctions) path
    case result of
        (Left err) -> throwE err
        (Right rendered) -> writeText rendered

renderExtends :: String -> [Piece] -> Render ()
renderExtends path ast = do
    let blocksMap = M.fromList $ overridenBlocks ast
    state <- getState
    result <- liftIO $ feedFromFile (defaultRenderState {globalVars = (globalVars state), blocksLookup=blocksMap}) path
    case result of
        (Left err) -> throwE err
        (Right rendered) -> writeText rendered
    where
        overridenBlocks = foldr blockFolder []
        blockFolder piece acc = case piece of {(BlockPiece i ps) -> (i, ps):acc; _ -> acc}

renderBlock :: String -> [Piece] -> Render ()
renderBlock blockId defPieces = do
    pushFrame
    state <- getState
    pieces <- maybe (return defPieces) return (M.lookup blockId $ blocksLookup state)
    render pieces
    popFrame
