{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Lens
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Function (on)
import Data.List (sortBy)
import Data.Map as M
import Data.Monoid
import Data.Set as S
import qualified Data.Text as T
import Data.Text.Lens
import Data.Time
import Development.Shake hiding (Resource)
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import Slick
import Control.Monad

outputFolder :: FilePath
outputFolder = "dist/"

main :: IO ()
main =
  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity = Chatty, shakeThreads=4}
   in shakeArgsForward shOpts $ do
    allPosts <- buildPosts
    allTags <- getTags allPosts
    buildTags allTags
    buildIndex allPosts allTags
    buildFeed allPosts
    copyStaticFiles

data IndexInfo = IndexInfo
  { indexPosts :: [Post]
  , indexTags :: [Tag]
  } deriving (Generic, Show)

instance FromJSON IndexInfo

instance ToJSON IndexInfo

data Tag = Tag
  { tag :: String
  , tagPosts :: [Post]
  , url :: String
  } deriving (Generic, Show)

instance FromJSON Tag

instance ToJSON Tag

data Post = Post
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , image :: Maybe String
  , tags :: [String]
  , nextPostURL :: Maybe String
  , prevPostURL :: Maybe String
  , isoDate :: String
  , date :: String
  , srcPath :: String
  , description :: String
  , slug :: String
  } deriving (Generic, Eq, Ord, Show, Binary)

instance FromJSON Post where
  parseJSON v = do
    let title = v ^. key "title" . _String . unpacked
        author = v ^. key "author" . _String . unpacked
        date = v ^. key "date" . _String . unpacked
        isoDate = formatDate date
        content = v ^. key "content" . _String . unpacked
        url = v ^. key "url" . _String . unpacked
        tags = v ^.. key "tags" . values . _String . unpacked
        nextPostURL = Nothing
        prevPostURL = Nothing
        srcPath = v ^. key "srcPath" . _String . unpacked
        image = v ^? key "image" . _String . unpacked
        description = v ^. key "description" . _String . unpacked
        slug = v ^. key "slug" . _String . unpacked
     in return Post {..}

instance ToJSON Post

postNames :: Action [FilePath]
postNames = getDirectoryFiles "." ["site/posts//*.md"]

destToSrc :: FilePath -> FilePath
destToSrc p = "site" </> dropDirectory1 p

srcToDest :: FilePath -> FilePath
srcToDest p = "dist" </> dropDirectory1 p

srcToURL :: FilePath -> String
srcToURL = ("/" ++) . dropDirectory1 . dropExtension

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

buildPosts :: Action [Post]
buildPosts = do
    pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
    forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
  let withPostUrl = _Object . at "url" ?~ String postUrl
  let withSlug =
        _Object . at "slug" ?~
        String (T.pack . dropExtension . takeBaseName $ srcPath)
  -- Add additional metadata we've been able to compute
  let fullPostData = withSlug . withPostUrl $ postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  convert fullPostData

-- buildIndex :: [Post] -> Action ()
-- buildIndex posts' = do
--   indexT <- compileTemplate' "site/templates/index.html"
--   let indexInfo = IndexInfo {posts = posts'}
--       indexHTML = T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
--   writeFile' (outputFolder </> "index.html") indexHTML

buildIndex :: [Post] -> [Tag] -> Action ()
buildIndex allPosts allTags = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {indexPosts=allPosts, indexTags=allTags}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

findPosts :: Action ()
findPosts = do
  pNames <- postNames
  need ((\p -> srcToDest p -<.> "html") <$> pNames)

buildTags :: [Tag] -> Action ()
buildTags tags = do
    void $ forP tags writeTag

writeTag :: Tag -> Action ()
writeTag t@Tag{tag} = do
  tagTempl <- compileTemplate' "site/templates/tag.html"
  writeFile' (outputFolder </> tag) . T.unpack $ substitute tagTempl (toJSON t)

getNeighbours :: String -> [String] -> (Maybe String, Maybe String)
getNeighbours i xs =
  let ms = pure <$> xs
   in go ([Nothing] <> ms <> [Nothing])
  where
    go (before:Just current:after:_)
      | current == i = (before, after)
    go (_:rest) = go rest
    go [] = (Nothing, Nothing)

getTags :: [Post] -> Action [Tag]
getTags posts = do
   let tagToPostsSet = M.unionsWith mappend (toMap <$> posts)
       tagToPostsList = fmap S.toList tagToPostsSet
       tagObjects =
         foldMapWithKey
           (\tag ps -> [Tag {tag, tagPosts = sortByDate ps, url = "/tag/" <> tag}])
           tagToPostsList
   return tagObjects
  where
    toMap :: Post -> Map String (Set Post)
    toMap p@Post {tags} = M.unionsWith mappend (embed p <$> tags)
    embed :: Post -> String -> Map String (Set Post)
    embed post tag = M.singleton tag (S.singleton post)

sortByDate :: [Post] -> [Post]
sortByDate = sortBy (flip compareDates)
  where
    compareDates = compare `on` isoDate

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:%SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { title = "Chris Penner"
          , domain = "https://chrispenner.ca"
          , author = "Chris Penner"
          , posts = posts
          , currentTime = toIsoDate now
          , url = "/atom.xml"
          }
  atomTempl <- compileTemplate' "site/templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ substitute atomTempl (toJSON atomData)

data AtomData = AtomData
  { title :: String
  , domain :: String
  , author :: String
  , posts :: [Post]
  , currentTime :: String
  , url :: String
  } deriving (Generic, Eq, Ord, Show)

instance ToJSON AtomData
