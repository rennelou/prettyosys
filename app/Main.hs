{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli

import Verify.SbyCommand
import Verify.SbyConfigFile.SbyConfigFile
import Verify.SbyConfigFile.GetSbyConfigFiles
import Verify.CoverPoint
import Verify.Assertion
import Verify.Error

import View.CoverTable
import qualified Data.ByteString.Lazy as BL

import System.IO
import System.Exit (ExitCode)
import System.Process.Typed
import Control.Concurrent.STM (atomically)

main :: IO ()
main = verifyAll

verifyAll :: IO ()
verifyAll = do
    args <- getCliOptions
    
    let createSbyConfigFiles = getCreateSbyConfigFiles args
    sbys <- createSbyConfigFiles "src" "verification_units"
    
    let createCommand = getCreateCommand args
    verifications <- 
        mapM
            (\ sby -> do
                putStrLn "Running verification"
                (prettyPrint . execute . createCommand) sby
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

execute :: String -> IO BL.ByteString
execute command = do
    (_, out, _) <- readProcess $ shell command
    return out

prettyPrint :: IO BL.ByteString -> IO ()
prettyPrint ioOut = do
    out <- ioOut
    let coverLogs = getCoverPoints out
    putStrLn $ show coverLogs
    
    let basecaseLogs = getBasecaseAssertion out
    putStrLn $ show basecaseLogs

    let inductionLogs = getInductionAssertion out
    putStrLn $ show inductionLogs

    let errorLogs = getError out
    putStrLn $ show errorLogs

    putStrLn $ createCoverTable coverLogs