{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import Verify.SbyCommand
import Verify.SbyConfigFile.GetSbyConfigFiles

import System.IO
import System.Exit (ExitCode)
import System.Process.Typed
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent.STM (atomically)

main :: IO ()
--main = getCliOptions >>= createSby >>= callCommand
main = do
    args <- getCliOptions
    let sbyCommandArgs = getSbyCommandArgs args
    let createCommand = sbyCommandWithConfigFile sbyCommandArgs
    
    let sbyConfigArgs = getSbyConfigArgs args
    sbys <- getSbyConfigFiles sbyConfigArgs "src" "verification_units"
    
    verifications <- 
        mapM
            (\ sby -> do
                putStrLn "Running verification"
                (execute . createCommand) sby
            )
            sbys
    return ()

execute :: String -> IO ()
execute command = do
    runProcess $ shell command
    return ()
