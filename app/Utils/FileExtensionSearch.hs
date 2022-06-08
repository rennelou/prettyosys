module Utils.FileExtensionSearch (
    getFilesAndPaths
) where

import Parsers.PSL

import System.Directory
import System.FilePath
import Data.Functor
import Control.Monad

getFilesAndPaths :: [String] -> FilePath -> IO ([FilePath], [FilePath])
getFilesAndPaths extensions rootDir = do
    (names, paths) <- getRecursiveContents rootDir <&> unzip
    let filtered_names = filterByExtesions extensions names
    let filtered_paths = filterByExtesions extensions paths
    return (filtered_names, filtered_paths)

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

filterByExtesions :: [String] -> [FilePath] -> [FilePath]
filterByExtesions extensions = filter (\ path -> any (`isExtensionOf` path) extensions)