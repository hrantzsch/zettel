{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Args where

import System.Console.CmdArgs

data Mode'
  = Overview
  | Stats
  | Validate
  deriving (Show, Data)

data Zettel = Zettel {path :: FilePath, mode :: Mode'}
  deriving (Show, Data, Typeable)

zettel =
  Zettel
    { path = "." &= typDir,
      mode =
        enum
          [ Overview &= help "Print overview",
            Stats &= help "Show stats",
            Validate &= help "Check stuff"
          ]
    }
    &= helpArg [explicit, name "h", name "help"]
    &= versionArg [explicit, name "v", name "version"]

parseArgs = cmdArgs zettel
