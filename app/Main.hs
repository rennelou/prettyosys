{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import Verify.SbyCommand
import Verify.SbyConfigFile.SbyConfigFile
import Verify.SbyConfigFile.GetSbyConfigFiles
import Verify.CoverPoint

import Parsers.SbyLog.SbyLog
import Text.Megaparsec hiding (State)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
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
    (_, out, err) <- readProcess $ shell command
    
    let coverLogs = getCoverPoints out
    putStrLn $ show coverLogs
    
    let basecaseLogs = getBasecaseLogs $ (T.decodeUtf8 . B.concat . BL.toChunks) out
    putStrLn $ show basecaseLogs

    let inductionLogs = getInductionLogs $ (T.decodeUtf8 . B.concat . BL.toChunks) out
    putStrLn $ show inductionLogs

    let errorLogs = getErrorLogs $ (T.decodeUtf8 . B.concat . BL.toChunks) out
    putStrLn $ show errorLogs

