{-# LANGUAGE OverloadedStrings #-}

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
getCoverLogs = mapMaybe getCover . parseLogs
    where
        getCover :: SbyLog -> Maybe Cover
        getCover SbyLogLine { taskPath=path, logline=(SolverType (CoverLog (WritingCoverVCD trace))) } =
            Just (WritingCoverVCD (path ++ "/" ++ trace))
        getCover SbyLogLine { taskPath=path, logline=(SolverType (CoverLog cover)) } = Just cover
        getCover _ = Nothing

getBasecaseLogs :: Text -> [Basecase]
getBasecaseLogs = mapMaybe getBaseCase . parseLogs
    where
        getBaseCase :: SbyLog -> Maybe Basecase
        getBaseCase SbyLogLine { taskPath=path, logline=(SolverType (BasecaseLog (BasecaseWritingVCD trace))) } =
            Just (BasecaseWritingVCD (path ++ "/" ++ trace))
        getBaseCase  SbyLogLine { taskPath=path, logline=(SolverType (BasecaseLog basecase)) } = Just basecase
        getBaseCase _ = Nothing

getInductionLogs :: Text -> [Induction]
getInductionLogs = mapMaybe getInduction . parseLogs
    where
        getInduction :: SbyLog -> Maybe Induction
        getInduction SbyLogLine { taskPath=path, logline=(SolverType (InductionLog (InductionWritingVCD trace))) } =
            Just (InductionWritingVCD (path ++ "/" ++ trace))
        getInduction SbyLogLine { taskPath=path, logline=(SolverType (InductionLog induction)) } =
            Just induction
        getInduction _ = Nothing

getErrorLogs :: Text -> [String]
getErrorLogs = mapMaybe errorFilter . parseLogs
    where
        errorFilter :: SbyLog -> Maybe String
        errorFilter (Error error) = Just error
        errorFilter _ = Nothing

parseLogs :: Text -> [SbyLog]
parseLogs logTxt =
    case logs of
        Nothing -> error "Error parsing symbiyosys log"
        Just parsedLogs -> parsedLogs
  where
    logs = parseMaybe pSbyLog logTxt

pSbyLog :: TextParser [SbyLog]
pSbyLog =
    choice [
        M.some (lexeme pError),
        M.some (lexeme pSbyLogLine) ]

pSbyLogLine :: TextParser SbyLog
pSbyLogLine = do
    path <- pSbyHeader
    SbyLogLine path <$> pLogType

pSbyHeader :: TextParser String
pSbyHeader = do
    _ <- pKeyword "SBY"
    _ <- pHour
    T.unpack <$> pBlock '[' ']' pPath

pError :: TextParser SbyLog
pError = do
    _ <- pKeyword "ERROR:"
    Error <$> (pAnything <* char '\n')