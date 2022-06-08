module Verify.Sby (
    Sby(..),
    SbyConfigArgs(..),
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
    topLevel    :: String,
    files       :: [String],
    paths       :: [String],
    configArgs  :: SbyConfigArgs
}

newtype SbyConfigArgs = SbyConfigArgs {
    depht       :: Int
}

instance Show Sby where
  show Sby {topLevel=topLevel, files=files, paths=paths, configArgs=args} =
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
        printf "cover: depth %d\n" (depht args) ++
        "prove: mode prove\n" ++
        printf "prove: depth %d\n" (depht args)

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

getSbys :: SbyConfigArgs-> String -> String -> IO [Sby]
getSbys args srcPath vunitPath = do
    srcPaths <- getFiles ["vhd", "vhdl"] srcPath
    let srcFileNames = getFileNames srcPaths
    vunits <- getVunits srcPath vunitPath
    return (
        map
            (\ (psl, file, path) ->
                Sby
                    (getTopLevel psl)
                    (srcFileNames ++ [file])
                    (srcPaths ++ [path])
                    args
            )
            vunits
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

tryExtractPSLFile :: String -> String -> String -> Maybe (PSLFile, String, String)
tryExtractPSLFile text file path = do
    psl <- (parseMaybe pPSL . T.pack) text
    return (psl, file, path)