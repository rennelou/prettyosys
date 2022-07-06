module Verify.Sby (
    Sby(..),
    getSbys
) where

import Parsers.PSL
import Utils.FileExtensionSearch

import Data.List
import System.FilePath
import qualified Data.Text as T
import Text.Printf
import Data.Maybe
import Text.Megaparsec hiding (State)

data Sby = Sby {
    topLevel :: String,
    files    :: [String],
    paths    :: [String],
    depht    :: Int
}

instance Show Sby where
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

getSbys :: String -> Int -> String -> String -> IO [Sby]
getSbys uut depht srcPath vunitPath = do
    
    srcPaths <- getFiles ["vhd", "vhdl"] srcPath
    let srcFileNames = getFileNames srcPaths
    
    vunits <- getVunits srcPath vunitPath
    let filteredVunits = filterByUut uut vunits
    let vunitFile = getVunitFiles vunits
    let vunitPaths = getVunitPaths vunits

    return (
        map
            (\ (psl, file, path) ->
                Sby
                    (getTopLevel psl)
                    (srcFileNames ++ vunitFile)
                    (srcPaths ++ vunitPaths)
                    depht
            )
            filteredVunits
        )

getVunits :: String -> String -> IO [(PSLFile, String, String)]
getVunits srcPath vunitPath = do
    srcPaths <- getFiles ["psl", "vhd", "vhdl"] srcPath
    testPaths <- getFiles ["psl", "vhd", "vhdl"] vunitPath
    let paths = srcPaths ++ testPaths
    catMaybes <$> mapM ( \ path ->
        do
            text <- readFile path
            let fileName = takeFileName path
            return (tryExtractPSLFile text fileName path) )
        paths
  where
    tryExtractPSLFile :: String -> String -> String -> Maybe (PSLFile, String, String)
    tryExtractPSLFile text file path = do
      psl <- (parseMaybe pPSL . T.pack) text
      return (psl, file, path)

filterByUut :: String -> [(PSLFile, String, String)] -> [(PSLFile, String, String)]
filterByUut "" vunits = vunits
filterByUut uut vunits = filter (\ (psl, file, path) -> getTopLevel psl == uut) vunits

getVunitFiles :: [(PSLFile, String, String)] -> [String]
getVunitFiles = map (\ (_, file, path) -> file)

getVunitPaths :: [(PSLFile, String, String)] -> [String]
getVunitPaths = map (\ (_, file, path) -> path)