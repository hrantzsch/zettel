{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-cse #-}

module Args (Mode(..), Zt(..), parseArgs) where

import System.Console.CmdArgs (Data, Typeable, cmdArgs, enum, explicit, help, helpArg, name, typDir, versionArg, (&=))

data Mode
  = Overview
  | Stats
  | Validate
  deriving (Show, Data)

data Zt = Zt {path :: FilePath, mode :: Mode}
  deriving (Show, Data, Typeable)

parseArgs :: IO Zt
parseArgs =
  cmdArgs
    ( Zt
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
    )
