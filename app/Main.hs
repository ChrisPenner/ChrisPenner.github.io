{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Arrow
import qualified Control.Arrow as Arrow
import qualified Control.Category as Cat
import Control.Category.Notation (cat, catDebugDesugaring)
import qualified Control.Category.Tensor as MF
import Control.Lens hiding ((#))
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Lens
import Data.Bifunctor
import Data.Function (on)
import Data.List (sortBy, stripPrefix)
import qualified Data.List as List
import Data.Map as Map
import Data.Maybe
import Data.Profunctor.Traversing (Traversing (..))
import Data.Set as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lens
import Data.Time
import Data.Time.Format.ISO8601
import Debug.Trace (trace)
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
import qualified "monoidal-functors" Control.Category.Cartesian as MF
import Prelude hiding (writeFile)

outputFolder :: FilePath
outputFolder = "dist/"

main :: IO ()
main = withForge catSyntaxSiteBuilder $ \(input, output) -> do
  putStrLn "Building..."
  _ <- atomically $ input ()
  forever $ do
    out <- atomically $ output
    print out
    putStrLn "Waiting for changes..."

-- traverseTest :: Forge () [Int]
-- traverseTest =
--   [cat|
-- \() -> do
--   (traverse' (delayBy <<< arr (\n -> (n, n)) <<< countup )) [1, 2, 3, 4]
--   |]

-- main :: IO ()
-- main = withForge f $ \(input, output) -> do
--   putStrLn "Building..."
--   _ <- atomically $ input ()
--   posts <- atomically $ output
--   print posts
--   where
--     -- putStrLn $ "Built " <> show (length posts) <> " posts"
--     f = proc () -> do watchGlob -< ("site/posts", ["**/*.md"])

voidArr :: (Arrow.Arrow a) => a i ()
voidArr = arr (const ())

forkTuple :: (MonadUnliftIO m) => Flinch m a b -> Flinch m a c -> Flinch m a (b, c)
forkTuple = MF.fork

fst' :: (MonadUnliftIO m) => Flinch m (a, b) a
fst' = MF.projl

snd' :: (MonadUnliftIO m) => Flinch m (a, b) b
snd' = MF.projr

(/\) :: (MonadUnliftIO m) => Flinch m a b -> Flinch m a c -> Flinch m a (b, c)
(/\) = (MF./\)

(#) :: (MonadUnliftIO m) => Flinch m a b -> Flinch m c d -> Flinch m (a, c) (b, d)
(#) = (MF.#)

arrNotationSiteBuilder :: Forge () ()
arrNotationSiteBuilder = proc () -> do
  (posts, ()) <- buildPosts &&& copyStaticFiles -< ()
  let sortedPosts = sortByDate posts
  let allTags = getTags posts
  () <- buildTags -< allTags
  () <- buildIndex -< (sortedPosts, allTags)
  buildFeed -< sortedPosts
  returnA -< ()

catSiteBuilder :: Forge () ()
catSiteBuilder =
  (buildPosts /\ copyStaticFiles)
    >>> MF.projl
    >>> rmap (\posts -> let sorted = sortByDate posts in (sorted, getTags sorted)) Cat.id
    >>> (((uniq >>> buildFeed) # (uniq >>> buildTags)) /\ (uniq >>> buildIndex))
    >>> MF.kill

arrCombinatorSiteBuilder :: Forge () ()
arrCombinatorSiteBuilder =
  (buildPosts &&& copyStaticFiles)
    >>> arr (\(posts, ()) -> let sorted = sortByDate posts in (sorted, getTags sorted))
    >>> (((uniq >>> buildFeed) *** (uniq >>> buildTags)) &&& (uniq >>> buildIndex))
    >>> arr (const ())

sortByDate' :: [Post] -> [Post]
sortByDate' = undefined

buildPosts' :: () -> [Post]
buildPosts' = undefined

getTags' :: [Post] -> [Tag]
getTags' = undefined

buildTags' :: [Tag] -> ()
buildTags' = undefined

buildIndex' :: ([Post], [Tag]) -> ()
buildIndex' = undefined

buildFeed' :: [Post] -> ()
buildFeed' = undefined

trace' :: (Show a) => Forge a a
trace' = mapIO (\a -> putStrLn ("trace: " <> show a) *> return a)

trace'' :: (Show a) => String -> Forge a a
trace'' label = mapIO (\a -> putStrLn (label <> ": " <> show a) *> return a)

tap :: (Show a) => String -> Forge a a
tap label = mapIO (\a -> putStrLn label *> pure a)

catSyntaxSiteBuilder :: Forge () ()
catSyntaxSiteBuilder =
  [catDebugDesugaring| \() -> do
               posts <- (tap "rebuilt posts" <<< buildPostsCat) ()
               -- () <- (tap "copied static" <<< copyStaticFilesCat) ()
               let sortedPosts = sortByDate (Map.elems posts)
               allTags <- (uniq <<< arr getTagsCat) sortedPosts
               () <- (buildTagsCat  <<< tap "rebuilding tags") allTags
               () <-  (buildIndexCat  <<< tap "rebuilding index" <<< threshold (Milliseconds 100)) (sortedPosts, Map.elems allTags)
               () <- (buildFeedCat  <<< tap "rebuilding feed") sortedPosts
               ()
         |]

-- arr (\ x -> (x, x)) >>>
--               first f >>> arr (\ (y, x) -> (x, y)) >>>
--               first g >>> arr (\ (z, y) -> y + z)

data IndexInfo = IndexInfo
  { indexPosts :: [Post],
    indexTags :: [Tag]
  }
  deriving (Generic, Show, Eq)

instance ToJSON IndexInfo where
  toJSON IndexInfo {..} =
    object
      [ "posts" A..= indexPosts,
        "tags" A..= indexTags
      ]

data Tag = Tag
  { tag :: String,
    tagUrl :: String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Tag where
  toJSON Tag {..} =
    object
      [ "tag" A..= tag,
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
  (filepaths, _) <- watchGlob -< ("site/", ["images/**/*", "css/**/*", "js/**/*"])
  rmap (const ()) (traverse' (mapIO (copyFile))) -< tracer "copying: " (Set.toList filepaths)
  where
    copyFile :: FilePath -> IO ()
    copyFile fp = do
      let srcPath = fp
          destPath = outputFolder </> fromMaybe fp (stripPrefix "site/" fp)
      putStrLn $ "Copying static file: " <> srcPath <> " -> " <> destPath
      Directory.createDirectoryIfMissing True (takeDirectory destPath)
      Directory.copyFile srcPath destPath
      putStrLn $ "Copied static file: " <> srcPath <> " -> " <> destPath

copyStaticFilesCat :: Forge () ()
copyStaticFilesCat =
  [cat| \() -> do
  (filepaths, _x) <- watchGlob ("site/", ["images/**/*", "css/**/*", "js/**/*"])
  _y <- (traverse' (mapIO copyFile)) (Set.toList filepaths)
  ()
  |]
  where
    copyFile :: FilePath -> IO ()
    copyFile fp = do
      let srcPath = fp
          destPath = outputFolder </> fromMaybe fp (stripPrefix "site/" fp)
      putStrLn $ "Copying static file: " <> srcPath <> " -> " <> destPath
      Directory.createDirectoryIfMissing True (takeDirectory destPath)
      Directory.copyFile srcPath destPath
      putStrLn $ "Copied static file: " <> srcPath <> " -> " <> destPath

buildPosts :: Forge () [Post]
buildPosts = proc () -> do
  (updated, _files) <- watchGlob -< ("site/posts", ["**/*.md"])
  traverse' buildPost -< Set.toList updated

buildPostsCat :: Forge () (Map.Map FilePath Post)
buildPostsCat =
  [cat| \() -> do
  (updated, files) <- watchGlob ("site/posts", ["**/*.md"])
  let taggedFiles =
        Set.toList files
          <&> (\fp -> (fp, (fp, fp `Set.member` updated)))
          & Map.fromList
  traverse' (buildPost <<< MF.projl <<< uniq) taggedFiles
  |]

writeFile :: FilePath -> T.Text -> IO ()
writeFile path contents = do
  Directory.createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path contents

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
        _Object
          . at "slug"
          ?~ String (T.pack . dropExtension . takeBaseName $ srcPath)
  -- Add additional metadata we've been able to compute
  let fullPostData = withSlug . withPostUrl $ postData
  template <- compileTemplate -< "site/templates/post.html"
  let postContents = substitute template fullPostData
  let postPath = (outputFolder </> T.unpack postUrl -<.> "html")
  () <- mapIO (uncurry writeFile) -< (postPath, postContents)
  convert -< fullPostData

buildIndex :: Forge ([Post], [Tag]) ()
buildIndex = proc (allPosts, allTags) -> do
  indexT <- compileTemplate -< "site/templates/index.html"
  let indexInfo = IndexInfo {indexPosts = allPosts, indexTags = allTags}
      indexHTML = substitute indexT (toJSON indexInfo)
  mapIO (uncurry writeFile) -< ((outputFolder </> "index.html"), indexHTML)

buildIndexCat :: Forge ([Post], [Tag]) ()
buildIndexCat =
  [cat| \(allPosts, allTags) -> do
  indexT <- compileTemplate "site/templates/index.html"
  let indexInfo = IndexInfo {indexPosts = allPosts, indexTags = allTags}
  let indexHTML = substitute indexT (toJSON indexInfo)
  mapIO (uncurry writeFile) ((outputFolder </> "index.html"), indexHTML)
  |]

buildTags :: Forge [Tag] ()
buildTags = proc tags -> do
  rmap (const ()) (traverse' writeTag) -< tags

buildTagsCat :: Forge (Map String Tag) ()
buildTagsCat = rmap (const ()) $ traverse' writeTagCat
  where
    writeTagCat :: Forge Tag ()
    writeTagCat =
      [cat| \t -> do
                  tagTempl <- compileTemplate "site/templates/tag.html"
                  let tagFilePath = outputFolder </> (List.dropWhile (== '/') (tagUrl t)) -<.> "html"
                  let tagContents = substitute tagTempl (toJSON t)
                  mapIO (uncurry writeFile) (tagFilePath, tagContents)
        |]

writeTag :: Forge Tag ()
writeTag = proc (t@Tag {tagUrl}) -> do
  tagTempl <- compileTemplate -< "site/templates/tag.html"
  let tagFilePath = outputFolder </> (List.dropWhile (== '/') tagUrl) -<.> "html"
  let tagContents = substitute tagTempl (toJSON t)
  mapIO (uncurry writeFile) -< (tagFilePath, tagContents)

getTags :: [Post] -> [Tag]
getTags = getTagsCat >>> Map.elems

getTagsCat :: [Post] -> (Map.Map String Tag)
getTagsCat posts =
  let tagToPostsSet = Map.unionsWith mappend (toMap <$> posts)
      tags = Map.keys tagToPostsSet
      tagObjects =
        tags & foldMap (\tag -> Map.singleton tag (Tag {tag, tagUrl = "/tag/" <> tag}))
   in tagObjects
  where
    toMap :: Post -> Map String (Set Post)
    toMap p@Post {postTags} = Map.unionsWith mappend (embed p <$> postTags)
    embed :: Post -> String -> Map String (Set Post)
    embed post tag = Map.singleton tag (S.singleton post)

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
  mapIO (uncurry writeFile) -< (atomFilePath, atomContents)

buildFeedCat :: Forge [Post] ()
buildFeedCat =
  [cat| \posts -> do
  now <- mapIO (const getCurrentTime) ()
  let atomData =
        AtomData
          { title = "Chris Penner",
            domain = "https://chrispenner.ca",
            author = "Chris Penner",
            posts = posts,
            currentTime = toIsoDate now,
            url = "/atom.xml"
          }
  atomTempl <- compileTemplate "site/templates/atom.xml"
  let atomFilePath = outputFolder </> "atom.xml"
  let atomContents = substitute atomTempl (toJSON atomData)
  mapIO (uncurry writeFile) (atomFilePath, atomContents)
  |]

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
compileTemplate = mapIO (fmap (Data.Bifunctor.first SomeException) . localAutomaticCompile) >>> reportException

tracer :: (Show a) => String -> a -> a
tracer label x = trace (label <> ": " <> show x) x
