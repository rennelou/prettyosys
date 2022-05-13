module Cli.RunApp (
    runApp
) where

import Cli.VerifyArgs
import Verify.Verify

import Control.Monad
import Options.Applicative

data Options = Options {
    optCommand :: Command
}

data Command = Verify VerifyArgs

runApp ::IO ()
runApp = join $ execParser 
    (info (opts <**> helper)
        (  fullDesc
        <> progDesc "Program for use Yosys without files input and configurations"
        <> header "prettyosys - Pretty Cli wrapper for Yosys" ) )

opts :: Parser (IO ())
opts = hsubparser 
    ( command "verify" (info (verifyAll <$> parseVerifyArgs) (progDesc "Execute formal verification") ) )

