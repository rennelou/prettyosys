module Main where

import Sby
import Cli
import SearchVHDLFiles
import Text.Printf
import System.Process


topLevel = "sorted_list"

main :: IO ()
main = do
    args <- getCliOptions
    command <- createSby args
    callCommand command

createSby :: Args -> IO String
createSby args = do
    (files, paths) <- getFilesAndPaths
    return ( printf "echo \"%s\" | %s" 
                (sbyConfig (getDepht args) files topLevel paths)
                (sbyCommand (getMode args) (getBackupFlag args) (getWorkDir args) topLevel) )