module Cli.VerifyArgs (
    parseVerifyArgs,
    VerifyArgs
) where

import Verify.Verify
import Cli.Utils
import Options.Applicative

parseVerifyArgs :: Parser VerifyArgs
parseVerifyArgs = VerifyArgs 
                    <$> parseMode
                    <*> parseBackupFlag
                    <*> parseReplaceFlag
                    <*> parseWorkDir
                    <*> parseDepht

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
    <> help "Backup old work directory before run verification" )

parseReplaceFlag :: Parser Bool
parseReplaceFlag = switch
    (  long "replace"
    <> short 'r'
    <> help "Remove old work directories before run verification" )