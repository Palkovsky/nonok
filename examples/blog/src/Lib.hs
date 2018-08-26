{-# LANGUAGE RecordWildCards #-}

module Lib
    ( generateBlog
    ) where

import Text.Nonok
import Data.Aeson
import System.Directory
import Data.Foldable (foldrM)

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as B

data Post = Post { title :: T.Text
                 , text  :: T.Text
                 , slug  :: T.Text}
                 deriving (Show)

instance FromJSON Post where
    parseJSON = withObject "post" $ \o -> do
        title <- o .: (T.pack "title")
        text  <- o .: (T.pack "text")
        let slug = slugify title
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
            return $ case maybePost of {Just x -> x:acc; _ -> acc}) [] items
    setCurrentDirectory ".."
    return posts
    where
        parsePost f = do
            contents <- B.readFile f
            return $ decode contents

generatePost :: Post -> IO ()
generatePost post = do
    let
      globals = M.fromList
          [("post", express $ M.fromList
              [ ("title", express $ T.unpack $ title post)
              , ("text", express $ T.unpack $ text post)
              , ("slug", express $ T.unpack $ slug post)]
          )]
    result <- feedFromFile globals "layout/post.html"
    case result of
        Right str -> writeFile ("generated/blog/" ++ (T.unpack $ slug post) ++ ".html") str
        Left err  -> putStrLn err

generateListing :: [Post] -> IO ()
generateListing posts = do
    let
      p = express $ map (\post -> express $
        M.fromList
          [ ("title", express $ T.unpack $ title post)
          , ("text", express $ T.unpack $ text post)
          , ("slug", express $ T.unpack $ slug post)]) posts
    let globals = M.fromList [("posts", p)]
    result <- feedFromFile globals "layout/listing.html"
    case result of
        Left err  -> putStrLn err
        Right str -> writeFile "generated/index.html" str

generateBlog :: IO ()
generateBlog = do
    removePathForcibly "generated"
    createDirectoryIfMissing True "generated/blog"
    posts <- parsePosts
    generateListing posts
    mapM_ generatePost posts
