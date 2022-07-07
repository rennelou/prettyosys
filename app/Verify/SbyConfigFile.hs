module Verify.SbyConfigFile (
  SbyConfigFile(..),
  createSymbiosysConfigFiles
) where

import Data.List
import System.FilePath
import qualified Data.Text as T
import Text.Printf
import Data.Maybe
import Text.Megaparsec hiding (State)

import Utils.Parsers.PSL
import Utils.FileExtensionSearch

data SbyConfigFile = Sby {
  topLevel :: String,
  files    :: [String],
  paths    :: [String],
  depht    :: Int
}

data PSLFile = PSLFile {
  getPsl      :: PSL,
  getFilename :: String,
  getPath     :: String
}

instance Show SbyConfigFile where
  show Sby {topLevel=topLevel, files=files, paths=paths, depht=depht} =
      sbyTasksConfig ++
      sbyEngineConfig ++
      sbyScriptsConfig ++
      sbyFilesConfig
    where
      sbyTasksConfig :: String
      sbyTasksConfig =
        "[tasks]\n" ++
        "cover\n" ++
        "prove\n" ++
        "[options]\n" ++
        "cover: mode cover\n" ++
        printf "cover: depth %d\n" depht ++
        "prove: mode prove\n" ++
        printf "prove: depth %d\n" depht

      sbyEngineConfig :: String
      sbyEngineConfig =
        "[engines]\n" ++
        "smtbmc\n"

      sbyScriptsConfig :: String
      sbyScriptsConfig =
        "[script]\n" ++
        printf "ghdl --std=08 %s -e %s\n" (unwords files) topLevel ++
        printf "prep -top %s\n" topLevel

      sbyFilesConfig :: String
      sbyFilesConfig = printf "[files]\n%s" (intercalate "\n" paths)

createSymbiosysConfigFiles :: String -> Int -> String -> String -> IO [SbyConfigFile]
createSymbiosysConfigFiles uut depht srcPath vunitPath = do  
  srcPaths <- getFiles ["vhd", "vhdl"] srcPath
  
  let srcFileNames = getFileNames srcPaths
   
  vunits <- getPropertySpecificationFiles srcPath vunitPath

  let filteredVunits = filterByUut uut vunits
  let vunitFile = map getFilename vunits
  let vunitPaths = map getPath vunits

  return (
    map
      (\ pslFile ->
        Sby
          (getTopLevel (getPsl pslFile))
          (srcFileNames ++ vunitFile)
          (srcPaths ++ vunitPaths)
          depht )
      filteredVunits )

getPropertySpecificationFiles :: String -> String -> IO [PSLFile]
getPropertySpecificationFiles srcPath vunitPath = do
  srcPaths <- getFiles ["psl", "vhd", "vhdl"] srcPath
  testPaths <- getFiles ["psl", "vhd", "vhdl"] vunitPath
  let paths = srcPaths ++ testPaths
  
  catMaybes <$>
    mapM 
      ( \ path -> do
        let fileName = takeFileName path
        text <- readFile path
        let maybePslFile = parsePsl $ T.pack text
        return (case maybePslFile of
          Just pslFile -> Just (PSLFile {getPsl=pslFile, getFilename =fileName, getPath=path})
          Nothing      -> Nothing ) )
      paths

filterByUut :: String -> [PSLFile] -> [PSLFile]
filterByUut "" vunits = vunits
filterByUut uut vunits = filter (\ pslFile -> getTopLevel (getPsl pslFile) == uut) vunits