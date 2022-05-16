module Cli.VerifyArgs (
    parseVerifyArgs,
    VerifyArgs
) where

import Verify.SbyCommand
import Cli.Utils
import Options.Applicative

parseVerifyArgs :: Parser VerifyArgs
parseVerifyArgs = VerifyArgs <$> parseMode <*> parseBackupFlag <*> parseWorkDir <*> parseDepht

parseMode :: Parser Mode
parseMode = option auto
    (  long "mode"
    <> short 'm'
    <> showDefault
    <> value CoverProve
    <> help "Mode cover, prove or cover and prove" )

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