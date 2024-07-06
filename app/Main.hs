{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Args
import Control.Monad (when)
import qualified Data.Bifunctor
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import qualified Data.MultiMap as MM
import Data.Ord (comparing)
import System.Directory
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.RE.TDFA.String (matchedText, matches, re, (*=~), (?=~))

type Tag = String

type Tags = MM.MultiMap Tag FilePath

fromLast :: (Eq a) => a -> [a] -> [a]
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
  tags <- extractTags <$> readFile zettelPath
  when (null tags) $ do
    hPutStrLn stderr $ "No tags found in '" ++ zettelPath ++ "'"
  return tags
  where
    extractTags :: String -> [Tag]
    extractTags = maybe [] matchTags . matchTagsLine
    matchTags :: String -> [Tag]
    matchTags s = matches $ s *=~ [re|@[0-9a-zA-Z_-]+|]
    matchTagsLine :: String -> Maybe String
    matchTagsLine s = matchedText $ s ?=~ [re|^tags: \[.*\]$|]

allTags :: [FilePath] -> IO Tags
allTags ztFiles = asMap <$> mapM getTagsWithPath ztFiles
  where
    getTagsWithPath :: FilePath -> IO (FilePath, [Tag])
    getTagsWithPath p = (p,) <$> getTags p
    asMap :: [(FilePath, [Tag])] -> Tags
    asMap tuples = MM.fromList [(tag, path) | (path, tags) <- tuples, tag <- tags]

printOverview :: Tags -> String
printOverview tagMap =
  printf
    "Found %d Tags in %d Zettel.\n\
    \\n\
    \Tags: [%s]\n\
    \\n\
    \Zettel by Tag:\n\
    \%s"
    (length tags)
    (length . L.nub . concat . MM.elems $ tagMap)
    (L.intercalate ", " tags) -- list of tags
    (L.intercalate "\n\n" (map showTag tags)) -- <tag>:\n<files>
  where
    tags = MM.keys tagMap
    showTag t = "  " ++ t ++ ":\n    " ++ L.intercalate "\n    " (map basename $ MM.lookup t tagMap)

printStats :: Tags -> String
printStats tags = L.intercalate "\n" $ map (uncurry (printf "%s: %d")) occurences
  where
    occurences = L.sortBy (comparing snd) . map (Data.Bifunctor.second length) . M.toList $ MM.toMap tags

run :: Zt -> IO ()
run (Zt path' mode') = case mode' of
  Overview -> putStrLn . printOverview =<< allTags =<< getZettels path'
  Stats -> putStrLn . printStats =<< allTags =<< getZettels path'
  Validate -> putStrLn "Not implemented yet"

main :: IO ()
main = parseArgs >>= run
