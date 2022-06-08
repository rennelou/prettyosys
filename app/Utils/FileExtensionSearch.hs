module Utils.FileExtensionSearch (
    getFileNames,
    getFiles
) where

import Parsers.PSL

import System.Directory
import System.FilePath
import Data.Functor
import Control.Monad

getFileNames :: [FilePath] -> [String]
getFileNames = map takeFileName

getFiles :: [String] -> FilePath -> IO [FilePath]
getFiles extensions rootDir = filterByExtesions extensions <$> getRecursiveContents rootDir 

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
    names <- listDirectory topDir
    paths <- mapM
                (\ name -> do
                    let path = topDir </> name
                    isDirectory <- doesDirectoryExist path
                    if isDirectory
                        then getRecursiveContents path
                        else ( do
                             absolutePath <- makeAbsolute path
                             return [absolutePath] ))
                names
    return (concat paths)

filterByExtesions :: [String] -> [FilePath] -> [FilePath]
filterByExtesions extensions = filter (\ path -> any (`isExtensionOf` path) extensions)