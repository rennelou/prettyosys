module SearchVHDLFiles (
    getFilesAndPaths
) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List

getFilesAndPaths :: IO (String, String)
getFilesAndPaths = do
    srcs <- getRecursiveContents "src"
    tests <- getRecursiveContents "verification_units"
    let (files, paths) = unzip $ tests ++ srcs
    return (intercalate " " files, intercalate "\n" paths)

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