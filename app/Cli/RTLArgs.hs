module Cli.RTLArgs (
    parseRTLArgs
) where

import Cli.Utils
import RTL.RTL
import Options.Applicative

parseRTLArgs :: Parser RTLArgs
parseRTLArgs = RTLArgs <$> parseTopLevel <*> parseWorkDir

parseTopLevel :: Parser String
parseTopLevel = strOption
    (  long "toplevel"
    <> short 't'
    <> help "Top Level Entity")