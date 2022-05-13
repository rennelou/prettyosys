module Cli.VerifyArgs (
    parseVerifyArgs,
    VerifyArgs
) where

import Verify.SbyCommand
import Options.Applicative

parseVerifyArgs :: Parser VerifyArgs
parseVerifyArgs = VerifyArgs <$> parseMode <*> parseBackupFlag <*> paseWorkDir <*> parseDepht

parseMode :: Parser Mode
parseMode = option auto
    (  long "mode"
    <> short 'm'
    <> showDefault
    <> value CoverProve
    <> help "Mode cover, prove or cover and prove" )

paseWorkDir :: Parser String
paseWorkDir = strOption
    (  long "workdir"
    <> short 'w'
    <> showDefault
    <> value "verify_build"
    <> help "Work Directory")

parseDepht :: Parser Int
parseDepht = option auto
    (  long "depht"
    <> short 'd'
    <> showDefault
    <> value 20
    <> help "Number of cycles of stimuli" )

parseBackupFlag :: Parser Bool
parseBackupFlag = switch
    (  long "backup"
    <> short 'b'
    <> help "Backup actual work directory before run verification" )