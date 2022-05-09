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

import Data.Text (Text)
import Data.Maybe

topLevel = "sorted_list"

main :: IO ()
--main = getCliOptions >>= createSby >>= callCommand
main = do
    args <- getCliOptions
    verifications <- getVerificationSets "src" "verification_units" >>= (mapM (createSby args))
    putStrLn $ show verifications

createSby :: Args -> (PSLFile, String, String) -> IO String
createSby args (vunit, files, paths) = do
    return ( printf "echo \"%s\" | %s" 
                (sbyConfig (getDepht args) files (getTopLevel vunit) paths)
                (sbyCommand (getMode args) (getBackupFlag args) (getWorkDir args) topLevel) )

getVerificationSets :: String -> String -> IO [(PSLFile, String, String)]
getVerificationSets srcPath vunitPath = do
    (srcFiles, srcPaths) <- getSrcs srcPath
    vunits <- getVunits srcPath vunitPath
    return (
        map
            (\ (psl, file, path) -> (psl, fileConcat srcFiles file, pathConcat srcPaths path) )
            vunits )

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