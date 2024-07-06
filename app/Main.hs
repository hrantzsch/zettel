{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Args
import qualified Data.List as L
import qualified Data.MultiMap as M
import Data.Traversable (forM)
import System.Directory
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.RE.TDFA.String (RE, matchedText, matches, re, (*=~), (?=~))

type Tag = String

type FileContent = String

type Tags = M.MultiMap Tag FilePath

fromLast :: Eq a => a -> [a] -> [a]
fromLast x = reverse . takeWhile (/= x) . reverse

extension :: FilePath -> String
extension = fromLast '.'

basename :: FilePath -> FilePath
basename = fromLast '/'

joinPath :: FilePath -> FilePath -> FilePath
joinPath l r = l ++ "/" ++ r

getZettels :: FilePath -> IO [FilePath]
getZettels f = map (joinPath f) . mdFiles <$> getDirectoryContents f
  where
    mdFiles :: [FilePath] -> [FilePath]
    mdFiles = filter ((== "md") . extension)

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
print_ tagMap =
  printf
    "Found %d Tags in %d Zettel.\n\
    \\n\
    \Tags: [%s]\n\
    \\n\
    \Zettel by Tag:\n\
    \%s"
    (length tags)
    (length . L.nub . concat . M.elems $ tagMap)
    (L.intercalate ", " tags) -- list of tags
    (L.intercalate "\n\n" (map showTag tags)) -- <tag>:\n<files>
  where
    tags = M.keys tagMap
    showTag t = "  " ++ t ++ ":\n    " ++ L.intercalate "\n    " (map basename $ M.lookup t tagMap)

printStats :: Tags -> String
printStats tags = printf "%s" (L.intercalate "\n" $ map usage $ M.keys tags)
  where
    usage :: Tag -> String
    usage tag = printf "%s: %d" tag (length $ M.lookup tag tags)

run :: Zettel -> IO ()
run (Zettel path' mode') = case mode' of
  Overview -> putStrLn . print_ =<< allTags =<< getZettels path'
  Stats -> putStrLn . printStats =<< allTags =<< getZettels path'
  Validate -> putStrLn "Not implemented yet"

main :: IO ()
main = do
  parseArgs >>= run
