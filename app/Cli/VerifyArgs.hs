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
                    <*> parseUUT
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

parseUUT :: Parser String
parseUUT = strOption
    (  long "uut"
    <> short 'u'
    <> value ""
    <> help "Unit under the test" )

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