{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import qualified Data.List as L
import qualified Data.MultiMap as M
import Data.Traversable (forM)
import System.Console.CmdArgs
import System.Directory (getDirectoryContents)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.RE.TDFA.String (RE, matchedText, matches, re, (*=~), (?=~))

type Tag = String

type FileContent = String

type Tags = M.MultiMap Tag FilePath

getZettels :: FilePath -> IO [FilePath]
getZettels f = mdFiles <$> getDirectoryContents f
  where
    mdFiles :: [FilePath] -> [FilePath]
    mdFiles = filter (\a -> ".md" == dropWhile (/= '.') a)

getTags :: FilePath -> IO [Tag]
getTags zettelPath = do
  f <- readFile zettelPath
  case extractTags f of
    [] -> do
      hPutStrLn stderr $ "No tags found in '" ++ zettelPath ++ "'"
      return []
    tags -> return tags
  where
    extractTags :: FileContent -> [Tag]
    extractTags f = case matchedText $ f ?=~ matchTagsLine of
      Just t -> matches $ t *=~ matchAllTags
      Nothing -> []

matchAllTags :: RE
matchAllTags = [re|@[0-9a-zA-Z_-]+|]

matchTagsLine :: RE
matchTagsLine = [re|^tags: \[.*\]$|]

asMap :: [(FilePath, [Tag])] -> Tags
asMap tuples = M.fromList $ concatMap f tuples
  where
    f :: (FilePath, [Tag]) -> [(Tag, FilePath)]
    f (_, []) = []
    f (k, v : vs) = (v, k) : f (k, vs)

allTags :: [FilePath] -> IO Tags
allTags paths = do
  tags <- forM paths $ \p -> do
    ts <- getTags p
    return (p, ts)
  return $ asMap tags

print_ :: Tags -> String
print_ m =
  printf
    "Found %d Tags in %d Zettel.\n\
    \\n\
    \Tags: [%s]\n\
    \\n\
    \Zettel by Tag:\n\
    \%s"
    (length tags)
    (length . L.nub . concat . M.elems $ m)
    (L.intercalate ", " tags) -- list of tags
    (L.intercalate "\n\n" (map showTag tags)) -- <tag>:\n<files>
  where
    tags = M.keys m
    showTag t = "  " ++ t ++ ":\n    " ++ L.intercalate "\n    " (M.lookup t m)

printStats :: Tags -> String
printStats tags = printf "%s" (L.intercalate "\n" $ map usage $ M.keys tags)
  where
    usage :: Tag -> String
    usage tag = printf "%s: %d" tag (length $ M.lookup tag tags)

data Mode' = Overview | Stats deriving (Show, Data)

data Zettel = Zettel {path :: FilePath, mode :: Mode'}
  deriving (Show, Data, Typeable)

zettel =
  Zettel
    { path = "." &= typDir,
      mode =
        enum
          [ Overview &= help "Print overview",
            Stats &= help "Show stats"
          ]
    }
    &= helpArg [explicit, name "h", name "help"]
    &= versionArg [explicit, name "v", name "version"]

main :: IO ()
main = do
  cmdArgs zettel >>= print

-- data Zettel
--   = List {path :: FilePath}
--   | Stats {path :: FilePath}
--   deriving (Show, Data, Typeable)
--
-- -- I could be better off using one "mode" and a "--mode" argument that defaults to summary
--
-- allModes :: [Zettel]
-- allModes =
--   [ List {path = def},
--     Stats {path = def}
--   ]
--
-- main :: IO ()
-- main = do
--   cmdArgs (modes allModes) >>= \case
--     List p -> putStrLn p
--     _ -> putStrLn "other"
