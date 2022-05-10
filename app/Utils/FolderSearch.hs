module Utils.FolderSearch (
    getFilesAndPaths
) where

import System.Directory
import System.FilePath
import Control.Monad

getFilesAndPaths :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndPaths rootDir = (getRecursiveContents rootDir) >>= return.unzip  

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