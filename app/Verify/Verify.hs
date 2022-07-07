{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Verify.Verify (
    VerifyArgs(..),
    Mode(..),
    runVerification
) where

import System.Directory
import qualified Data.Text as T

import Verify.Commands
import Verify.Sby

data VerifyArgs = VerifyArgs {
        getMode :: Mode,
        getTopLevel :: String,
        getBackupFlag :: Bool,
        getReplaceFlag :: Bool,
        getWorkDir :: String,
        getDepht :: Int
    } deriving (Show);

runVerification :: VerifyArgs -> IO ()
runVerification args = do
    let toplevel = getTopLevel args
    let depht = getDepht args
    let workdir = getWorkDir args
    let replaceFlag = getReplaceFlag args
    let mode = getMode args
    let backupFlag = getBackupFlag args

    sbyConfigFiles <- createSymbiosysConfigFiles toplevel depht "src" "verification_units"

    createDirectoryIfMissing True workdir
    setCurrentDirectory workdir

    mapM_
        (\ sbyConfigFile -> do
            
            removeOldDirectoriesWhen replaceFlag toplevel
            
            putStrLn $ "\n\t\t\t" ++ toplevel ++ " verification"
            
            putStrLn $ "\n" ++ " linting " ++ toplevel ++ "...\n"
            lint sbyConfigFile
            
            putStrLn $ "\n" ++ " verifing " ++ toplevel ++ "...\n"
            sbyLog <- verify mode backupFlag toplevel sbyConfigFile

            prettyPrint workdir sbyLog
        )
        sbyConfigFiles