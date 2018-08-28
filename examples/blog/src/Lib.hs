{-# LANGUAGE RecordWildCards #-}

module Lib
    ( generateBlog
    ) where

import Text.Nonok
import Data.Aeson
import CMarkGFM (commonmarkToHtml, optSafe)
import System.Directory
import Data.Foldable (foldrM)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as B

data Post = Post { title :: T.Text
                 , textPath :: T.Text
                 , ttr :: Integer
                 , text  :: T.Text
                 , slug  :: T.Text}
                 deriving (Show)

instance FromJSON Post where
    parseJSON = withObject "post" $ \o -> do
        title <- o .: (T.pack "title")
        textPath  <- o .: (T.pack "markdown")
        ttr <- o .: (T.pack "ttr")
        let slug = slugify title
            text = T.singleton ' '
        return Post{..}

-- this one is a very bad slugifier
slugify :: T.Text -> T.Text
slugify = T.toLower . T.replace (T.singleton ' ') (T.singleton '-')

parsePosts :: IO [Post]
parsePosts = do
    setCurrentDirectory "entries"
    items <- listDirectory "."
    posts <- foldrM
        (\f acc -> do
            maybePost <- parsePost f
            return $ maybe acc (\x -> x:acc) maybePost) [] items
    postsWithContent <- mapM parseMarkdown posts
    setCurrentDirectory ".."
    return postsWithContent
    where
        parseMarkdown post = do
            t <- TIO.readFile $ T.unpack $ textPath post
            return $ post {text = commonmarkToHtml [optSafe] [] t}
        parsePost f = do
            contents <- B.readFile f
            return $ decode contents

postToExpr :: Post -> Expression
postToExpr post = express $ M.fromList
    [ ("title", express $ T.unpack $ title post)
    , ("text", express $ T.unpack $ text post)
    , ("slug", express $ T.unpack $ slug post)
    , ("ttr", expressInt $ ttr post)]

generatePost :: Post -> IO ()
generatePost post = do
    let globals = M.fromList [("post", postToExpr post)]
    result <- feedFromFile (initialRenderState globals) "layout/post.html"
    case result of
        Right txt -> TIO.writeFile ("generated/blog/" ++ (T.unpack $ slug post) ++ ".html") txt
        Left err  -> putStrLn $ show err

generateListing :: [Post] -> IO ()
generateListing posts = do
    let globals = M.fromList [("posts", express $ map postToExpr posts)]
    result <- feedFromFile (initialRenderState globals) "layout/listing.html"
    case result of
        Right txt -> TIO.writeFile "generated/index.html" txt
        Left err  -> putStrLn $ show err

generateBlog :: IO ()
generateBlog = do
    removePathForcibly "generated"
    createDirectoryIfMissing True "generated/blog"
    posts <- parsePosts
    generateListing posts
    mapM_ generatePost posts
