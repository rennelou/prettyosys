module Main where

import Verify
import Cli
import FolderSearch
import Parsers.PSL
import Parsers.FileExtensions.VHDL
import Parsers.FileExtensions.PSL

import System.IO
import System.Process
import Text.Printf
import Data.List
import Text.Megaparsec hiding (State)
import qualified Data.Text as T

topLevel = "sorted_list"

main :: IO ()
--main = getCliOptions >>= createSby >>= callCommand
main = readFile "verification_units/vunit_sorted_list.psl" >>= (parseTest pPSL) . T.pack

createSby :: Args -> IO String
createSby args = do
    (srcFiles, srcPaths) <- getFilesAndPaths "src"
    (testFiles, testPaths) <- getFilesAndPaths "verification_units"
    let files = intercalate " " $ (filterVHDLExtension srcFiles) ++ (filterPSLExtension testFiles)
    let paths = intercalate "\n" $ (filterVHDLExtension srcPaths) ++ (filterPSLExtension testPaths)
    return ( printf "echo \"%s\" | %s" 
                (sbyConfig (getDepht args) files topLevel paths)
                (sbyCommand (getMode args) (getBackupFlag args) (getWorkDir args) topLevel) )