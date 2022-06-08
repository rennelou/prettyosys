module Verify.Sby (
    Sby(..),
    SbyConfigArgs(..),
    getSbys
) where

import Parsers.PSL
import Utils.FileExtensionSearch

import Data.List
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
    (srcFiles, srcPaths) <- getVHDLSrcs srcPath
    vunits <- getVunits srcPath vunitPath
    return (
        map
            (\ (psl, file, path) ->
                Sby
                    (getTopLevel psl)
                    (srcFiles ++ [file])
                    (srcPaths ++ [path])
                    args
            )
            vunits
        )

getVHDLSrcs :: String -> IO ([String], [String])
getVHDLSrcs = getFilesAndPaths ["vhd", "vhdl"]

getVunits :: String -> String -> IO [(PSLFile, String, String)]
getVunits srcPath vunitPath = do
    (srcFiles, srcPaths) <- getFilesAndPaths ["psl", "vhd", "vhdl"] srcPath
    (testFiles, testPaths) <- getFilesAndPaths ["psl", "vhd", "vhdl"] vunitPath
    let files = srcFiles ++ testFiles
    let paths = srcPaths ++ testPaths
    let pairs = zip files paths
    catMaybes <$> mapM ( \ (file, path) ->
        do
            text <- readFile path
            return (tryExtractPSLFile text file path) )
          pairs

tryExtractPSLFile :: String -> String -> String -> Maybe (PSLFile, String, String)
tryExtractPSLFile text file path =
    if isJust maybePSL
        then do
            psl <- maybePSL
            Just (psl, file, path)
        else
            Nothing

    where
        maybePSL = (parseMaybe pPSL . T.pack) text