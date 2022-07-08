module Init.Init (
  InitArgs(..),
  initProject
) where

import System.IO
import System.Directory
import qualified Data.Text as T 

import Settings.Settings

data InitArgs = InitArgs {
  workDirArg    :: String,
  srcDirArg     :: String,
  vunitsDirArgs :: String,
  dephtArg      :: Int
}

initProject :: InitArgs -> IO ()
initProject initArgs = do
  createDirectoryIfMissing True (srcDirArg initArgs)
  createDirectoryIfMissing True (vunitsDirArgs initArgs)
  writeFile settingsFilename (T.unpack $ createSettings (convertToSettings initArgs)) 

convertToSettings :: InitArgs -> Settings
convertToSettings initArgs = 
  Settings 
    (T.pack $ workDirArg initArgs)
    (T.pack $ srcDirArg initArgs)
    (T.pack $ vunitsDirArgs initArgs)
    (dephtArg initArgs)