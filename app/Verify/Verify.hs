{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

import Utils.FileExtensionSearch

import View.CoverTable
import View.AssertionTable
import qualified Data.ByteString.Lazy as BL

import System.IO
import System.Directory
import System.Process.Typed
import Control.Monad

data VerifyArgs = VerifyArgs {
        getMode :: Mode,
        getBackupFlag :: Bool,
        getReplaceFlag :: Bool,
        getWorkDir :: String,
        getDepht :: Int
    } deriving (Show);

verifyAll :: VerifyArgs -> IO ()
verifyAll args = do
    sbys <- getSbys (getDepht args) "src" "verification_units"
    createDirectoryIfMissing True (getWorkDir args)
    setCurrentDirectory (getWorkDir args)
    verifications <-
        mapM
            (\ sby -> do

                removeOldDirectoriesWhenHolds (getReplaceFlag args) (topLevel sby)

                putStrLn $ "\n\t\t\t" ++ topLevel sby ++ " verification"

                putStrLn $ "\n" ++ " linting " ++ topLevel sby ++ "...\n"
                callCommand $ yosysLint sby

                putStrLn $ "\n" ++ " verifing " ++ topLevel sby ++ "...\n"
                verify args sby
            )
            sbys

    return ()

verify :: VerifyArgs -> Sby -> IO ()
verify args sby = do

    let symbiyosys' = Commands.symbiyosys (SbyCommandArgs (getMode args) (getBackupFlag args))
    out <- (readCommand . symbiyosys') sby
    prettyPrint out

  where
    prettyPrint :: BL.ByteString -> IO ()
    prettyPrint out = do
      putStrLn "\n\t\t\tCover Points\n"
      putStrLn $ createCoverTable $ getCoverPoints out

      putStrLn "\n\t\t\tAssertions\n"
      putStrLn $ createAssertionTable
          (  getCoverAssertion out
          ++ getBasecaseAssertion out
          ++ getInductionAssertion out )

      putStrLn $ getError out
      putStrLn "\n"

readCommand :: String -> IO BL.ByteString
readCommand command = do
  (_, out, _) <- readProcess $ shell command
  return out

callCommand :: String -> IO ()
callCommand = runProcess_ . shell

removeOldDirectoriesWhenHolds :: Bool -> String -> IO ()
removeOldDirectoriesWhenHolds flag topLevel =
    when flag (do 
      tryRemoveDirectory (topLevel ++ "_cover")
      tryRemoveDirectory (topLevel ++ "_prove")
      return ())