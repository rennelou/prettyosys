{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SbyLog (
    pSbyLog
) where

import Parsers.SbyLog.SbyCommand
import Parsers.SbyLog.SolverLog
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
    logline     :: LogType
} deriving (Show)

data LogType = SbyType SbyCommand | SolverType SolverLog deriving (Show)

pSbyLog :: TextParser [SbyLogLine]
pSbyLog = M.some (lexeme pSbyLogLine)

pSbyLogLine :: TextParser SbyLogLine
pSbyLogLine = do
    path <- pSbyHeader
    logline <- pLogType
    return (SbyLogLine path logline)

pLogType :: TextParser LogType
pLogType = choice [
        try pSbyType,
        pSolverType
    ]

pSbyType :: TextParser LogType
pSbyType = do
    sbyCommand <- pSbyCommand
    return (SbyType sbyCommand)

pSolverType :: TextParser LogType
pSolverType = do
    solverType <- pSolverLog
    return (SolverType solverType)

pSbyHeader :: TextParser String
pSbyHeader = do
    _ <- pKeyword "SBY"
    hour <- integer
    _ <- char ':'
    minute <- integer
    _ <- char ':'
    seconds <- integer
    T.unpack <$> pBlock '[' ']' pPath