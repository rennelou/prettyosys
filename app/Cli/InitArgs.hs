module Cli.InitArgs (
  parseInitArgs
) where

import Init.Init
import Options.Applicative

parseInitArgs :: Parser InitArgs
parseInitArgs = InitArgs 
               <$> parseBeginWorkDir
               <*> parseSrcDir
               <*> parseVunitsDir
               <*> parseBeginDepht

parseBeginWorkDir :: Parser String
parseBeginWorkDir = strOption
  (  long "workdir"
  <> short 'w'
  <> showDefault
  <> value "verify_build"
  <> help "Work Directory")

parseSrcDir :: Parser String
parseSrcDir = strOption
  (  long "srcdir"
  <> short 's'
  <> showDefault
  <> value "src"
  <> help "Src Directory")

parseVunitsDir :: Parser String
parseVunitsDir = strOption
  (  long "vunitsdir"
  <> short 'v'
  <> showDefault
  <> value "vunits"
  <> help "Verification Units Directory")

parseBeginDepht :: Parser Int
parseBeginDepht = option auto
  (  long "depht"
  <> short 'd'
  <> showDefault
  <> value 20
  <> help "Number of cycles of stimuli" )