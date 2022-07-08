{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Verify.Verify (
    VerifyArgs(..),
    Mode(..),
    runVerification
) where

import System.Directory
import qualified Data.Text as T

import Verify.Commands
import Verify.SbyConfigFile
import Settings.Settings

data VerifyArgs = VerifyArgs {
        getMode :: Mode,
        getUUT :: String,
        getBackupFlag :: Bool,
        getReplaceFlag :: Bool,
        getWorkDir :: String,
        getDepht :: Int
    } deriving (Show);

runVerification :: VerifyArgs -> IO ()
runVerification args = do
    settings <- getSettings
    
    let workDirSettings = T.unpack $ settingsWorkDir settings
    let srcDir = T.unpack $ settingsSrcDir settings
    let vunitsDir = T.unpack $ settingsVunitsDir settings
    let dephtSettings = settingsDepht settings

    let workdirArgs = getWorkDir args
    let dephtArgs = getDepht args
    let uut = getUUT args
    let replaceFlag = getReplaceFlag args
    let mode = getMode args
    let backupFlag = getBackupFlag args

    let workdir = ifDefaultGetAnother "" workdirArgs workDirSettings
    let depht = ifDefaultGetAnother 0 dephtArgs dephtSettings

    sbyConfigFiles <- createSymbiosysConfigFiles uut depht srcDir vunitsDir

    createDirectoryIfMissing True workdir
    setCurrentDirectory workdir

    mapM_
        (\ sbyConfigFile -> do
            let toplevel = topLevel sbyConfigFile

            removeOldDirectoriesWhen replaceFlag toplevel
            
            putStrLn $ "\n\t\t\t" ++ toplevel ++ " verification"
            
            putStrLn $ "\n" ++ " linting " ++ toplevel ++ "...\n"
            lint sbyConfigFile
            
            putStrLn $ "\n" ++ " verifing " ++ toplevel ++ "...\n"
            sbyLog <- verify mode backupFlag sbyConfigFile

            prettyPrint workdir sbyLog
        )
        sbyConfigFiles

ifDefaultGetAnother :: (Eq a) => a -> a -> a -> a
ifDefaultGetAnother defaultValue value1 value2 =
  if value1 /= defaultValue then
    value1
  else
    value2