module Verify.Verify (
    VerifyArgs(..),
    Mode(..),
    verifyAll
) where

import Verify.Commands as Commands
import Verify.Sby
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

data VerifyArgs = VerifyArgs {
        getMode :: Mode,
        getBackupFlag :: Bool,
        getWorkDir :: String,
        getDepht :: Int
    } deriving (Show);

verifyAll :: VerifyArgs -> IO ()
verifyAll args = do
    sbys <- getSbys (SbyConfigArgs (getDepht args)) "src" "verification_units"
    verifications <- 
        mapM
            (\ sby -> do
                putStrLn $ "\n\t\t\t" ++ (topLevel sby) ++ " verification"

                putStrLn $ "\n" ++ " linting " ++ (topLevel sby) ++ "..."
                callCommand $ ghdlLint sby

                putStrLn $ "\n" ++ " verifing " ++ (topLevel sby) ++ "..."
                verify args sby
            )
            sbys
    
    return ()

verify :: VerifyArgs -> Sby -> IO ()
verify args sby = do
  
    let symbiyosys' = Commands.symbiyosys (SbyCommandArgs (getMode args) (getBackupFlag args) (getWorkDir args))
    out <- (readCommand . symbiyosys') sby
    prettyPrint out
  
  where
    prettyPrint :: BL.ByteString -> IO ()
    prettyPrint out = do
      putStrLn $ createCoverTable $ getCoverPoints out
      putStrLn $ createAssertionTable (getBasecaseAssertion out) (getInductionAssertion out)

      putStrLn $ getError out
      putStrLn "\n"

readCommand :: String -> IO BL.ByteString
readCommand command = do
  (_, out, _) <- readProcess $ shell command
  return out

callCommand :: String -> IO ()
callCommand = runProcess_ . shell