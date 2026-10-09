{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Category ((>>>))
import Control.Lens
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Bifunctor
import Data.Function (on)
import Data.List (sortBy)
import Data.Map as M
import Data.Profunctor.Traversing (Traversing (..))
import Data.Set as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lens
import Data.Time
import Data.Time.Format.ISO8601
import Development.Shake.Classes
import Development.Shake.FilePath
import Flinch
import Forge
import GHC.Generics (Generic)
import Site.Pandoc
import Text.Mustache (Template)
import Text.Mustache.Compile (localAutomaticCompile)
import Text.Mustache.Render (substitute)
import UnliftIO
import qualified UnliftIO.Directory as Directory

outputFolder :: FilePath
outputFolder = "dist/"

main :: IO ()
main = withForge siteBuilder $ \(input, output) -> do
  _ <- atomically $ input ()
  forever $ do
    atomically $ output

siteBuilder :: Forge () ()
siteBuilder = proc () -> do
  allPosts <- buildPosts -< ()
  let sortedPosts = sortByDate allPosts
  let allTags = getTags allPosts
  () <- buildTags -< allTags
  () <- buildIndex -< (sortedPosts, allTags)
  () <- buildFeed -< sortedPosts
  copyStaticFiles -< ()

data IndexInfo = IndexInfo
  { indexPosts :: [Post],
    indexTags :: [Tag]
  }
  deriving (Generic, Show)

instance ToJSON IndexInfo where
  toJSON IndexInfo {..} =
    object
      [ "posts" A..= indexPosts,
        "tags" A..= indexTags
      ]

data Tag = Tag
  { tag :: String,
    tagPosts :: [Post],
    tagUrl :: String
  }
  deriving (Generic, Show)

instance ToJSON Tag where
  toJSON Tag {..} =
    object
      [ "tag" A..= tag,
        "posts" A..= tagPosts,
        "url" A..= tagUrl
      ]

data Post = Post
  { postTitle :: String,
    postAuthor :: String,
    postContent :: String,
    postUrl :: String,
    postImage :: Maybe String,
    postTags :: [String],
    postNextPostURL :: Maybe String,
    postPrevPostURL :: Maybe String,
    postIsoDate :: String,
    postDate :: String,
    postSrcPath :: String,
    postDescription :: String,
    postSlug :: String
  }
  deriving (Generic, Eq, Ord, Show, Binary)

instance FromJSON Post where
  parseJSON v = do
    let postTitle = v ^. key "title" . _String . unpacked
        postAuthor = v ^. key "author" . _String . unpacked
        postDate = v ^. key "date" . _String . unpacked
        postIsoDate = formatDate postDate
        postContent = v ^. key "content" . _String . unpacked
        postUrl = v ^. key "url" . _String . unpacked
        postTags = v ^.. key "tags" . values . _String . unpacked
        postNextPostURL = Nothing
        postPrevPostURL = Nothing
        postSrcPath = v ^. key "srcPath" . _String . unpacked
        postImage = v ^? key "image" . _String . unpacked
        postDescription = v ^. key "description" . _String . unpacked
        postSlug = v ^. key "slug" . _String . unpacked
     in return Post {..}

instance ToJSON Post where
  toJSON Post {..} =
    object
      [ "title" A..= postTitle,
        "author" A..= postAuthor,
        "content" A..= postContent,
        "url" A..= postUrl,
        "image" A..= postImage,
        "tags" A..= postTags,
        "nextPostURL" A..= postNextPostURL,
        "prevPostURL" A..= postPrevPostURL,
        "isoDate" A..= postIsoDate,
        "date" A..= postDate,
        "srcPath" A..= postSrcPath,
        "description" A..= postDescription,
        "slug" A..= postSlug
      ]

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Forge () ()
copyStaticFiles = proc () -> do
  -- cacheAction ("static-files" :: T.Text) $ do
  (filepaths, _) <- watchGlob -< ("site/", ["images//*", "css//*", "js//*"])
  rmap (const ()) (traverse' (mapIO (copyFile))) -< (Set.toList filepaths)
  where
    copyFile :: FilePath -> IO ()
    copyFile fp = do
      let srcPath = "site" </> fp
          destPath = outputFolder </> fp
      Directory.copyFile srcPath destPath

buildPosts :: Forge () [Post]
buildPosts = proc () -> do
  (updated, _files) <- watchGlob -< (".", ["site/posts//*.md"])
  traverse' buildPost -< Set.toList updated

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: Forge FilePath Post
buildPost = proc srcPath -> do
  {-cacheAction ("build" :: T.Text, srcPath) $ do -}
  postContent <- mapIO TIO.readFile -< srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTML -< postContent
  let postUrl = T.pack . dropDirectory1 . dropExtension $ srcPath
  let withPostUrl = _Object . at "url" ?~ String ("/" <> postUrl)
  let withSlug =
        _Object . at "slug"
          ?~ String (T.pack . dropExtension . takeBaseName $ srcPath)
  -- Add additional metadata we've been able to compute
  let fullPostData = withSlug . withPostUrl $ postData
  template <- compileTemplate -< "site/templates/post.html"
  let postContents = substitute template fullPostData
  let postPath = (outputFolder </> T.unpack postUrl -<.> "html")
  () <- mapIO (uncurry TIO.writeFile) -< (postPath, postContents)
  convert -< fullPostData

buildIndex :: Forge ([Post], [Tag]) ()
buildIndex = proc (allPosts, allTags) -> do
  indexT <- compileTemplate -< "site/templates/index.html"
  let indexInfo = IndexInfo {indexPosts = allPosts, indexTags = allTags}
      indexHTML = T.unpack $ substitute indexT (toJSON indexInfo)
  mapIO (uncurry writeFile) -< ((outputFolder </> "index.html"), indexHTML)

buildTags :: Forge [Tag] ()
buildTags = proc tags -> do
  rmap (const ()) (traverse' writeTag) -< tags

writeTag :: Forge Tag ()
writeTag = proc (t@Tag {tagUrl}) -> do
  tagTempl <- compileTemplate -< "site/templates/tag.html"
  let tagFilePath = outputFolder </> tagUrl -<.> "html"
  let tagContents = substitute tagTempl (toJSON t)
  mapIO (uncurry TIO.writeFile) -< (tagFilePath, tagContents)

getTags :: [Post] -> [Tag]
getTags posts =
  let tagToPostsSet = M.unionsWith mappend (toMap <$> posts)
      tagToPostsList = fmap S.toList tagToPostsSet
      tagObjects =
        foldMapWithKey
          (\tag ps -> [Tag {tag, tagPosts = sortByDate ps, tagUrl = "/tag/" <> tag}])
          tagToPostsList
   in tagObjects
  where
    toMap :: Post -> Map String (Set Post)
    toMap p@Post {postTags} = M.unionsWith mappend (embed p <$> postTags)
    embed :: Post -> String -> Map String (Set Post)
    embed post tag = M.singleton tag (S.singleton post)

sortByDate :: [Post] -> [Post]
sortByDate = sortBy (flip compareDates)
  where
    compareDates = compare `on` postIsoDate

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

toIsoDate :: UTCTime -> String
toIsoDate = formatShow (utcTimeFormat (calendarFormat ExtendedFormat) (timeOfDayFormat ExtendedFormat))

buildFeed :: Forge [Post] ()
buildFeed = proc posts -> do
  now <- mapIO (const getCurrentTime) -< ()
  let atomData =
        AtomData
          { title = "Chris Penner",
            domain = "https://chrispenner.ca",
            author = "Chris Penner",
            posts = posts,
            currentTime = toIsoDate now,
            url = "/atom.xml"
          }
  atomTempl <- compileTemplate -< "site/templates/atom.xml"
  let atomFilePath = outputFolder </> "atom.xml"
  let atomContents = substitute atomTempl (toJSON atomData)
  mapIO (uncurry TIO.writeFile) -< (atomFilePath, atomContents)

data AtomData = AtomData
  { title :: String,
    domain :: String,
    author :: String,
    posts :: [Post],
    currentTime :: String,
    url :: String
  }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON AtomData where
  toJSON AtomData {..} =
    object
      [ "title" A..= title,
        "domain" A..= domain,
        "author" A..= author,
        "posts" A..= posts,
        "currentTime" A..= currentTime,
        "url" A..= url
      ]

compileTemplate :: Forge FilePath Template
compileTemplate = mapIO (fmap (first SomeException) . localAutomaticCompile) >>> reportException
