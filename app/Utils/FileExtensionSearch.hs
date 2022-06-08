module Utils.FileExtensionSearch (
    getVHDLSrcs,
    getVunits
) where

import Parsers.PSL

import System.Directory
import System.FilePath
import Data.Functor
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Text.Megaparsec hiding (State)


getVHDLSrcs :: String -> IO ([String], [String])
getVHDLSrcs srcPath = do
    (files_arr, paths_arr) <- getFilesAndPaths srcPath
    let files = filterVHDLExtension files_arr
    let paths = filterVHDLExtension paths_arr
    return (files, paths)

filterVHDLExtension :: [FilePath] -> [FilePath]
filterVHDLExtension =
    filter (\ path -> 
               isExtensionOf "vhd" path
            || isExtensionOf "vhdl" path )

getVunits :: String -> String -> IO [(PSLFile, String, String)]
getVunits srcPath vunitPath = do
    (srcFiles, srcPaths) <- getFilesAndPaths srcPath
    (testFiles, testPaths) <- getFilesAndPaths vunitPath
    let files = filterPSLExtension srcFiles ++ filterPSLExtension testFiles
    let paths = filterPSLExtension srcPaths ++ filterPSLExtension testPaths
    let pairs = zip files paths
    catMaybes <$> mapM ( \ (file, path) ->
        do
            text <- readFile path
            return (tryExtractPSLFile text file path) )
          pairs

filterPSLExtension :: [FilePath] -> [FilePath]
filterPSLExtension = 
    filter (\ path -> 
              isExtensionOf "psl" path
           || isExtensionOf "vhd" path
           || isExtensionOf "vhdl" path )

getFilesAndPaths :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndPaths rootDir = getRecursiveContents rootDir <&> unzip

getRecursiveContents :: FilePath -> IO [(FilePath,FilePath)]
getRecursiveContents topDir = do
    names <- listDirectory topDir
    paths <- mapM
                (\ name -> do
                    let path = topDir </> name
                    isDirectory <- doesDirectoryExist path
                    if isDirectory
                        then getRecursiveContents path
                        else return [(name, path)] )
                names
    return (concat paths)

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
    