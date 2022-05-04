module Cli (
    Args(..),
    getCliOptions
) where

import Sby
import Options.Applicative

data Args = Args {
    getMode :: Mode,
    getWorkDir :: String,
    getDepht :: Int,
    getBackupFlag :: Bool
} deriving (Show);

getCliOptions :: IO Args
getCliOptions = execParser opts

opts :: ParserInfo Args
opts = info (parseArgs <**> helper)
    (  fullDesc
    <> progDesc "Execute formal verification"
    <> header "hard-to-prove - Pretty Formal Verification Cli Tool" )

parseArgs :: Parser Args
parseArgs = Args <$> parseMode <*> paseWorkDir <*> parseDepht <*> parseBackupFlag

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