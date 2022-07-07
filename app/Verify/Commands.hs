module Verify.Commands (
    Mode(..),
    lint,
    verify,
    removeOldDirectoriesWhen,
    prettyPrint
) where

import Control.Monad
import Data.List
import Text.Printf
import System.Process.Typed
import System.IO
import System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T

import Verify.Sby
import Utils.FileExtensionSearch
import Verify.Types.CoverPoint
import Verify.Types.Assertion
import Verify.Types.Error
import Verify.View.CoverTable
import Verify.View.AssertionTable


data Mode = CoverProve | Cover | Prove deriving (Read, Show);

lint :: SymbiosysConfigFile -> IO ()
lint = callCommand . yosysLint

yosysLint :: SymbiosysConfigFile -> String
yosysLint sby =
  "yosys -m ghdl -qp " ++
  "'" ++
  "ghdl --std=08 " ++ unwords (paths sby) ++
  " -e "++ topLevel sby ++"; " ++
  "prep -top " ++ topLevel sby ++ "; " ++
  "hierarchy -simcheck" ++
  "'"


verify :: Mode -> Bool -> String -> SymbiosysConfigFile -> IO BL.ByteString
verify mode backupFlag toplevel sbyCongigFile = 
  readCommand
  $ symbiyosys mode backupFlag toplevel sbyCongigFile

symbiyosys :: Mode -> Bool -> String -> SymbiosysConfigFile -> String
symbiyosys mode backupFlag toplevel sbyConfigFile =
  printf "echo \"%s\" | sby --yosys \"yosys -m ghdl\" %s %s --prefix %s"
    (show sbyConfigFile)
    (sbyMode mode)
    (sbyBackupFlag backupFlag)
    toplevel


removeOldDirectoriesWhen :: Bool -> String -> IO ()
removeOldDirectoriesWhen flag topLevel =
  when flag (do
    tryRemoveDirectory (topLevel ++ "_cover")
    tryRemoveDirectory (topLevel ++ "_prove")
    return ())


prettyPrint :: String -> BL.ByteString -> IO ()
prettyPrint workdir out = do
  let log = (T.decodeUtf8 . B.concat . BL.toChunks) out
  putStrLn "\n\t\t\tCover Points\n"
  putStrLn $ createCoverTable $ getCoverPoints workdir log
  putStrLn "\n\t\t\tAssertions\n"
  putStrLn $ createAssertionTable
      (  getCoverAssertion     workdir log
      ++ getBasecaseAssertion  workdir log
      ++ getInductionAssertion workdir log )
  putStrLn $ getError workdir log
  putStrLn "\n"
  


sbyMode :: Mode -> String
sbyMode Cover = "-T cover"
sbyMode Prove = "-T prove"
sbyMode _ = ""

sbyBackupFlag :: Bool -> String
sbyBackupFlag False = ""
sbyBackupFlag True = "-b"



readCommand :: String -> IO BL.ByteString
readCommand command = do
  (_, out, _) <- readProcess $ shell command
  return out

callCommand :: String -> IO ()
callCommand = runProcess_ . shell