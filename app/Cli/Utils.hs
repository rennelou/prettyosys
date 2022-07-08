module Cli.Utils (
    parseWorkDir,
    parseDepht
) where

import Options.Applicative

parseWorkDir :: Parser String
parseWorkDir = strOption
  (  long "workdir"
  <> short 'w'
  <> value ""
  <> help "Work Directory")

parseDepht :: Parser Int
parseDepht = option auto
  (  long "depht"
  <> short 'd'
  <> value 0
  <> help "Number of cycles of stimuli" )