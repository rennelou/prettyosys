module Cli.InitArgs (
  parseInitArgs
) where

import Cli.Utils
import Init.Init
import Options.Applicative

parseInitArgs :: Parser InitArgs
parseInitArgs = InitArgs 
               <$> parseBeginWorkDir
               <*> parseSrcDir
               <*> parseVunitsDir
               <*> parseDepht

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