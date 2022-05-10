module Verify.SbyConfigFile.GetSbyConfigFiles (
    getSbyConfigFiles
) where

import Verify.SbyConfigFile.SbyConfigFile
import FolderSearch
import Parsers.PSL
import Parsers.FileExtensions.VHDL
import Parsers.FileExtensions.PSL

import Text.Megaparsec hiding (State)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Maybe

getSbyConfigFiles :: SbyConfigArgs-> String -> String -> IO [SbyConfigFile]
getSbyConfigFiles args srcPath vunitPath = do
    (srcFiles, srcPaths) <- getSrcs srcPath
    vunits <- getVunits srcPath vunitPath
    return (
        map
            (\ (psl, file, path) -> 
                SbyConfigFile 
                    (getTopLevel psl)
                    (fileConcat srcFiles file)
                    (pathConcat srcPaths path)
                    args 
            )
            vunits 
        )

getSrcs :: String -> IO (String, String)
getSrcs srcPath = do
    (files_arr, paths_arr) <- getFilesAndPaths srcPath
    let files = intercalate " " $ filterVHDLExtension files_arr
    let paths = intercalate "\n" $ filterVHDLExtension paths_arr
    return (files, paths)

getVunits :: String -> String -> IO [(PSLFile, String, String)]
getVunits srcPath vunitPath = do
    (srcFiles, srcPaths) <- getFilesAndPaths srcPath
    (testFiles, testPaths) <- getFilesAndPaths vunitPath
    let files = (filterPSLExtension srcFiles) ++ (filterPSLExtension testFiles)
    let paths = (filterPSLExtension srcPaths) ++ (filterPSLExtension testPaths)
    let pairs = zip files paths
    pslMaybes <- ( mapM
            ( \ (file, path) -> do
                text <- readFile path
                return (tryExtractPSLFile text file path) )
            pairs )
    return (catMaybes pslMaybes)

tryExtractPSLFile :: String -> String -> String -> Maybe (PSLFile, String, String)
tryExtractPSLFile text file path =
    if isJust maybePSL
        then do
            psl <- maybePSL
            Just (psl, file, path)
        else
            Nothing

    where
        maybePSL = ((parseMaybe pPSL) . T.pack) text
    
fileConcat :: String -> String -> String
fileConcat files file = files ++ " " ++ file

pathConcat :: String -> String -> String
pathConcat files file = files ++ "\n" ++ file