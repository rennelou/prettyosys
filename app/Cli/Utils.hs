module Cli.Utils (
    parseWorkDir
) where

import Options.Applicative

parseWorkDir :: Parser String
parseWorkDir = strOption
    (  long "workdir"
    <> short 'w'
    <> showDefault
    <> value "verify_build"
    <> help "Work Directory")