{-# LANGUAGE RecordWildCards #-}

module Lib
    ( generateBlog
    ) where

import Text.Nonok
import Data.Aeson
import CMarkGFM (commonmarkToHtml, optSafe)
import System.Directory
import Data.Foldable (foldrM)
import Control.Exception (throwIO)
import Control.Monad (when, mapM_)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as B

data Post = Post { title :: T.Text
                 , slug :: T.Text -- slug of title
                 , textPath :: T.Text -- path to markdown file linked with post
                 , description :: T.Text -- description of article, showed in listing
                 , ttr :: Integer -- estaminated time to read
                 , text  :: T.Text}
                 deriving (Show)

instance FromJSON Post where
    parseJSON = withObject "post" $ \o -> do
        title <- o .: (T.pack "title")
        textPath  <- o .: (T.pack "markdown")
        ttr <- o .: (T.pack "ttr")
        description <- o .: (T.pack "description")
        let slug = slugify title
            text = T.singleton ' ' -- we can't read markdown content here, because we're not in IO monad
        return Post{..}

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM monad app = monad >>= flip when app

eitherM :: (Monad m) => (a -> m c) -> (b -> m c) -> Either a b -> m c
eitherM f g e = either f g e

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory dirpath targetdir = do
    whenM (not <$> doesDirectoryExist dirpath) $ throwIO (userError "Source directory does not exist")
    whenM (doesDirectoryExist targetdir) $ throwIO (userError "Target directory already exists")
    createDirectory targetdir
    files <- listDirectory dirpath
    mapM_ (\f -> do
        let copySrc = dirpath ++ "/" ++ f
            copyTarget = targetdir ++ "/" ++ f
        whenM (doesDirectoryExist copySrc) $ copyDirectory copySrc copyTarget
        whenM (doesFileExist copySrc) $ copyFile copySrc copyTarget) files

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
    , ("description", express $ T.unpack $ description post)
    , ("ttr", expressInt $ ttr post)]

generatePost :: Post -> IO ()
generatePost post = do
    let globals = M.fromList [("post", postToExpr post), ("nav", express "post")]
    result <- feedFromFile (initialRenderState globals) "layout/partials/post.html"
    eitherM (throwIO . userError . show) (TIO.writeFile ("generated/blog/" ++ (T.unpack $ slug post) ++ ".html")) result

generateListing :: [Post] -> IO ()
generateListing posts = do
    let globals = M.fromList [("posts", express $ map postToExpr posts), ("nav", express "list")]
    result <- feedFromFile (initialRenderState globals) "layout/listing.html"
    eitherM (throwIO . userError . show) (TIO.writeFile "generated/listing.html") result

generateFrontpage :: IO ()
generateFrontpage = do
  let globals = M.fromList [("nav", express "index")]
  result <- feedFromFile (initialRenderState globals) "layout/index.html"
  eitherM (throwIO . userError . show) (TIO.writeFile "generated/index.html") result

generateBlog :: IO ()
generateBlog = do
    removePathForcibly "generated"
    createDirectoryIfMissing True "generated/blog"
    copyDirectory "layout/css" "generated/css"
    copyDirectory "layout/js" "generated/js"
    generateFrontpage
    posts <- parsePosts
    generateListing posts
    mapM_ generatePost posts
