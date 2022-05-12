{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SbyLog (
    pSbyLog
) where

import Parsers.SbyLog.SolverLog.SolverLog
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

data LogType = SbyCommand | SolverType SolverLog deriving (Show)

pSbyLog :: TextParser [SbyLogLine]
pSbyLog = M.some (lexeme pSbyLogLine)

pSbyLogLine :: TextParser SbyLogLine
pSbyLogLine = do
    path <- pSbyHeader
    logline <- pLogType
    return (SbyLogLine path logline)

pLogType :: TextParser LogType
pLogType = choice [
        pSolverType,
        pSbyCommand
    ]

pSolverType :: TextParser LogType
pSolverType = do
    solverType <- pSolverLog
    return (SolverType solverType)

pSbyHeader :: TextParser String
pSbyHeader = do
    _ <- pKeyword "SBY"
    _ <- pHour
    T.unpack <$> pBlock '[' ']' pPath

pSbyCommand :: TextParser LogType
pSbyCommand = lexeme pLine

pLine :: TextParser LogType
pLine = SbyCommand <$ (pAnything <* char '\n')

pAnything :: TextParser String
pAnything = M.many (satisfy (/= '\n'))