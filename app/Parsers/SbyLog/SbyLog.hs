{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SbyLog (
    getCoverLogs,
    getBasecaseLogs,
    getInductionLogs,
    getErrorLogs
) where

import Parsers.SbyLog.Utils
import Parsers.SbyLog.LogType.LogType
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

data SbyLog = 
      SbyLogLine { taskPath :: String, logline :: LogType }
    | Error String deriving (Show)

getCoverLogs :: Text -> [Cover]
getCoverLogs = getFilteredLogs (coverFilter <=< sbyLogLineFilter)
    where
        coverFilter :: LogType -> Maybe Cover
        coverFilter (SolverType (CoverLog cover)) = Just cover
        coverFilter _ = Nothing

getBasecaseLogs :: Text -> [Basecase]
getBasecaseLogs = getFilteredLogs (basecaseFilter <=< sbyLogLineFilter)
    where
        basecaseFilter :: LogType -> Maybe Basecase
        basecaseFilter (SolverType (BasecaseLog basecase)) = Just basecase
        basecaseFilter _ = Nothing

getInductionLogs :: Text -> [Induction]
getInductionLogs = getFilteredLogs (inductionFilter <=< sbyLogLineFilter)
    where
        inductionFilter :: LogType -> Maybe Induction
        inductionFilter (SolverType (InductionLog induction)) = Just induction
        inductionFilter _ = Nothing

getErrorLogs :: Text -> [String]
getErrorLogs = getFilteredLogs errorFilter
    where
        errorFilter :: SbyLog -> Maybe String
        errorFilter (Error error) = Just error
        errorFilter _ = Nothing

getFilteredLogs :: (SbyLog -> Maybe a) -> Text -> [a]
getFilteredLogs f = (catMaybes.map f) . parseLogs

sbyLogLineFilter :: SbyLog -> Maybe LogType
sbyLogLineFilter SbyLogLine { taskPath=_, logline=line } = Just line
sbyLogLineFilter _ = Nothing

parseLogs :: Text -> [SbyLog]
parseLogs = (fromMaybe []) . (parseMaybe pSbyLog)

pSbyLog :: TextParser [SbyLog]
pSbyLog = 
    choice [
        M.some (lexeme pError),
        M.some (lexeme pSbyLogLine) ]

pSbyLogLine :: TextParser SbyLog
pSbyLogLine = do
    path <- pSbyHeader
    logline <- pLogType
    return (SbyLogLine path logline)

pSbyHeader :: TextParser String
pSbyHeader = do
    _ <- pKeyword "SBY"
    _ <- pHour
    T.unpack <$> pBlock '[' ']' pPath

pError :: TextParser SbyLog
pError = do
    _ <- pKeyword "ERROR:"
    error <- (pAnything <* char '\n')
    return (Error error)