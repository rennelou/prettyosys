module Main where

import Cli
import Verify.SbyCommand
import Verify.SbyConfigFile.GetSbyConfigFiles

import System.IO
import System.Process

main :: IO ()
--main = getCliOptions >>= createSby >>= callCommand
main = do
    args <- getCliOptions
    let sbyCommandArgs = getSbyCommandArgs args
    let command = sbyCommandWithConfigFile sbyCommandArgs
    
    let sbyConfigArgs = getSbyConfigArgs args
    sbys <- getSbyConfigFiles sbyConfigArgs "src" "verification_units"
    --verifications <- mapM command verification_units
    putStrLn $ show sbys