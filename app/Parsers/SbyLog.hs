{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog (
    pSbyLogLine
) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as M
import Parsers.TextParser

data SbyLogLine = SbyLogLine {
    taskPath    :: String,
    cmd         :: LogType,
    arguments   :: String
} deriving (Show)

data LogType = Moving deriving (Show)

pSbyLogLine :: TextParser SbyLogLine
pSbyLogLine = do
    _ <- pKeyword "SBY"
    hour <- integer
    _ <- char ':'
    minute <- integer
    _ <- char ':'
    seconds <- integer
    path <- T.unpack <$> pBlock '[' ']' pPath
    cmd <- pLogType
    return (SbyLogLine path cmd "")

pLogType :: TextParser LogType
pLogType = 
    lexeme ( choice [
        Moving <$ pKeyword "Moving"
    ] )