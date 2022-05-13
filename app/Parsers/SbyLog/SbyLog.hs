{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SbyLog (
    getCoverLogs,
    getBasecaseLogs,
    getInductionLogs,
    getErrorLogs,
    Cover(..),
    Basecase(..),
    Induction(..)
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
getCoverLogs = catMaybes . (map getCover) . parseLogs
    where
        getCover :: SbyLog -> Maybe Cover
        getCover SbyLogLine { taskPath=path, logline=(SolverType (CoverLog (WritingCoverVCD trace))) } =
            Just (WritingCoverVCD (path ++ "/" ++ trace))
        getCover SbyLogLine { taskPath=path, logline=(SolverType (CoverLog cover)) } = Just cover
        getCover _ = Nothing

getBasecaseLogs :: Text -> [Basecase]
getBasecaseLogs = catMaybes . (map getBaseCase) . parseLogs
    where
        getBaseCase :: SbyLog -> Maybe Basecase
        getBaseCase SbyLogLine { taskPath=path, logline=(SolverType (BasecaseLog (AssertionWritingVCD trace))) } =
            Just (AssertionWritingVCD (path ++ "/" ++ trace))
        getBaseCase  SbyLogLine { taskPath=path, logline=(SolverType (BasecaseLog basecase)) } = Just basecase
        getBaseCase _ = Nothing

getInductionLogs :: Text -> [Induction]
getInductionLogs = catMaybes . (map getInduction) . parseLogs
    where
        getInduction :: SbyLog -> Maybe Induction
        getInduction SbyLogLine { taskPath=path, logline=(SolverType (InductionLog induction)) } =
            Just induction
        getInduction _ = Nothing

getErrorLogs :: Text -> [String]
getErrorLogs = (catMaybes.map errorFilter) . parseLogs
    where
        errorFilter :: SbyLog -> Maybe String
        errorFilter (Error error) = Just error
        errorFilter _ = Nothing

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