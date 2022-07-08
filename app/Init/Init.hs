module Init.Init (
  InitArgs(..),
  initProject
) where

import System.IO

data InitArgs = InitArgs {
  workDirArg    :: String,
  srcDirArg     :: String,
  vunitsDirArgs :: String,
  dephtArg      :: Int
}

initProject :: InitArgs -> IO ()
initProject initArgs = putStrLn "begin"