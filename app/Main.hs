module Main where

import Verify
import Cli
import FolderSearch
import Parsers.VHDL.Extension

import System.Process
import Text.Printf
import Data.List

topLevel = "sorted_list"

main :: IO ()
main = getCliOptions >>= createSby >>= callCommand

createSby :: Args -> IO String
createSby args = do
    (srcFiles, srcPaths) <- getFilesAndPaths "src"
    (testFiles, testPaths) <- getFilesAndPaths "verification_units"
    let files = intercalate " " $ filterVHDLExtension (srcFiles ++ testFiles)
    let paths = intercalate "\n" $ filterVHDLExtension (srcPaths ++ testPaths)
    return ( printf "echo \"%s\" | %s" 
                (sbyConfig (getDepht args) files topLevel paths)
                (sbyCommand (getMode args) (getBackupFlag args) (getWorkDir args) topLevel) )