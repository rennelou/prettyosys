module Cli.RunApp (
    runApp
) where

import Control.Monad
import Options.Applicative

import Cli.InitArgs
import Cli.VerifyArgs
import Cli.RTLArgs
import Init.Init
import Verify.Verify
import RTL.RTL

newtype Options = Options {
    optCommand :: Command
}

data Command = Verify VerifyArgs | RTL String

runApp ::IO ()
runApp = join $ execParser 
  (info (opts <**> helper)
    (  fullDesc
    <> progDesc "Program for use Yosys without files input and configurations"
    <> header "prettyosys - Pretty Cli wrapper for Yosys" ) )

opts :: Parser (IO ())
opts = hsubparser 
  (  command "init"   (info (initProject     <$> parseInitArgs)   (progDesc "Bootstrap a initial project") )
  <> command "verify" (info (runVerification <$> parseVerifyArgs) (progDesc "Execute formal verification") )
  <> command "rtl"    (info (generateRTL     <$> parseRTLArgs)    (progDesc "Generate RTL") ) )