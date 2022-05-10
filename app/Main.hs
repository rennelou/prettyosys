{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import Verify.SbyCommand
import Verify.SbyConfigFile.SbyConfigFile
import Verify.SbyConfigFile.GetSbyConfigFiles

import System.IO
import System.Exit (ExitCode)
import System.Process.Typed
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent.STM (atomically)

main :: IO ()
main = do
    args <- getCliOptions
    
    let createSbyConfigFiles = getCreateSbyConfigFiles args
    sbys <- createSbyConfigFiles "src" "verification_units"
    
    let createCommand = getCreateCommand args
    verifications <- 
        mapM
            (\ sby -> do
                putStrLn "Running verification"
                (execute . createCommand) sby
            )
            sbys
    
    return ()

getCreateCommand :: Args-> SbyConfigFile -> String
getCreateCommand = sbyCommandWithConfigFile.getSbyCommandArgs
    where
        getSbyCommandArgs :: Args -> SbyCommandArgs
        getSbyCommandArgs Args{getMode=mode, getBackupFlag=backup, getWorkDir=workDir, getDepht=_} = 
            SbyCommandArgs mode backup workDir

getCreateSbyConfigFiles :: Args -> String -> String -> IO [SbyConfigFile]
getCreateSbyConfigFiles = getSbyConfigFiles . getSbyConfigArgs
    where
        getSbyConfigArgs :: Args -> SbyConfigArgs
        getSbyConfigArgs args =
            SbyConfigArgs (getDepht args)

execute :: String -> IO ()
execute command = do
    runProcess $ shell command
    return ()