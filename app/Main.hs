{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Relude

import Data.Aeson
import Data.Time
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import Slick

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
  SiteMeta
    { siteAuthor = "Matt Friede"
    , baseUrl = "http://haskelldiaspora.com"
    , siteTitle = "Haskell Diaspora"
    , twitterHandle = Nothing
    , githubUser = Just "Friede80"
    }

outputFolder :: FilePath
outputFolder = "out/"

mergeObjects :: Value -> Value -> Value
mergeObjects (Object o1) (Object o2) = Object $ o1 <> o2
mergeObjects v1 _ = v1

--Data models-------------------------------------------------------------------

data SiteMeta = SiteMeta
  { siteAuthor :: String
  , baseUrl :: String
  , siteTitle :: String
  , twitterHandle :: Maybe String
  , githubUser :: Maybe String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON)

-- | Data for the index page
newtype IndexInfo = IndexInfo
  { posts :: [Post]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Tag = String

-- | Data for a blog post
data Post = Post
  { title :: String
  , author :: String
  , content :: String
  , url :: String
  , date :: String
  , tags :: [Tag]
  , description :: String
  , image :: Maybe String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, Binary)

data AtomData = AtomData
  { title :: String
  , domain :: String
  , author :: String
  , posts :: [Post]
  , currentTime :: String
  , atomUrl :: String
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (ToJSON)

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo{posts = posts'}
  let indexWithMeta = toJSON indexInfo `mergeObjects` toJSON siteMeta
  let indexHTML = substitute indexT indexWithMeta
  writeFile' (outputFolder </> "index.html") (toString indexHTML)

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

{- | Load a post, process metadata, write it to output, then return the post object
 Detects changes to either post content or template
-}
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: Text, srcPath) $ do
  putStrLn $ "Rebuilding post: " <> srcPath
  
  postData <- markdownToHTML . toText =<< readFile' srcPath
  
  let postUrl = dropDirectory1 $ srcPath -<.> "html"
    
  let fullPostData =
        postData
          `mergeObjects` object ["url" .= postUrl]
          `mergeObjects` toJSON siteMeta
  
  template <- compileTemplate' "site/templates/post.html"

  let fullPost = toString $ substitute template fullPostData
  writeFile' (outputFolder </> postUrl) fullPost
  
  convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
  void $
    forP filepaths $ \filepath ->
      copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
 where
  parsedTime =
    parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:SZ"))

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { title = siteTitle siteMeta
          , domain = baseUrl siteMeta
          , author = siteAuthor siteMeta
          , posts = mkAtomPost <$> posts
          , currentTime = toIsoDate now
          , atomUrl = "/atom.xml"
          }
  atomTempl <- compileTemplate' "site/templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") . toString $ substitute atomTempl (toJSON atomData)
 where
  mkAtomPost :: Post -> Post
  mkAtomPost p = p{date = formatDate $ date p}

{- | Specific build rules for the Shake system
   defines workflow to build the website
-}
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildIndex allPosts
  buildFeed allPosts
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = shakeOptions{shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules
  