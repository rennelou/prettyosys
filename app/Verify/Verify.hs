module Verify.Verify (
    verifyAll
) where

import Cli
import Verify.SbyCommand
import Verify.SbyConfigFile.SbyConfigFile
import Verify.SbyConfigFile.GetSbyConfigFiles
import Verify.CoverPoint
import Verify.Assertion
import Verify.Error

import View.CoverTable
import View.AssertionTable
import qualified Data.ByteString.Lazy as BL

import System.IO
import System.Exit (ExitCode)
import System.Process.Typed
import Control.Concurrent.STM (atomically)

verifyAll :: Args -> IO ()
verifyAll args = do
    let createSbyConfigFiles = getCreateSbyConfigFiles args
    sbys <- createSbyConfigFiles "src" "verification_units"
    
    let createCommand = getCreateCommand args
    verifications <- 
        mapM
            (\ sby -> do
                putStrLn "\n"
                putStrLn $ "\t\t\t" ++ (topLevel sby) ++ " verification"
                out <- (execute . createCommand) sby
                prettyPrint out
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

prettyPrint :: BL.ByteString -> IO ()
prettyPrint out = do
    putStrLn $ createCoverTable $ getCoverPoints out
    putStrLn $ createAssertionTable (getBasecaseAssertion out) (getInductionAssertion out)

    putStrLn $ getError out
    putStrLn "\n"