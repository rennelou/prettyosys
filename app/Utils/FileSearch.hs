module Utils.FileSearch (
    getVHDLSrcs,
    getVunits
) where

import Utils.FolderSearch
import Parsers.PSL
import Parsers.FileExtensions.VHDL
import Parsers.FileExtensions.PSL

import Data.List
import Data.Maybe
import qualified Data.Text as T
import Text.Megaparsec hiding (State)

getVHDLSrcs :: String -> IO (String, String)
getVHDLSrcs srcPath = do
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
    