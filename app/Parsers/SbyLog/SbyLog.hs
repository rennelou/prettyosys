{-# LANGUAGE OverloadedStrings #-}

module Parsers.SbyLog.SbyLog (
    getCoverLogs,
    getAssertionLogs,
    getErrorLogs,
    pSbyLog,
    SbyLog(..),
    CoverLog(..),
    AssertionLog(..)
) where

import System.FilePath
import Parsers.SbyLog.Utils
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
import Verify.Sby (Sby)

data SbyLog =
      SbyLogLine LogLine
    | AnyLine
    | Error String deriving (Show)

data LogLine = 
      CoverLine CoverLog
    | AssertionLine AssertionLog deriving (Show)

data CoverLog = 
      CoverpointReached String Integer
    | CoverpointUnreachd String
    | CoverpointVCD String 
    | CoverPointFail String String Integer deriving (Show)

data AssertionLog =
      AssertionStatus String String
    | AssertionStep String Integer
    | AssertionFail String String String
    | AssertionVCD String String deriving (Show)

getCoverLogs :: FilePath -> Text -> [CoverLog]
getCoverLogs currentDirectory = mapMaybe getCover . parseLogs
    
getCover :: SbyLog -> Maybe CoverLog
getCover (SbyLogLine (CoverLine cover)) = Just cover
getCover _ = Nothing


getAssertionLogs :: FilePath -> Text -> [AssertionLog]
getAssertionLogs currentDirectory = mapMaybe getAssertion . parseLogs

getAssertion :: SbyLog -> Maybe AssertionLog
getAssertion (SbyLogLine (AssertionLine assertion)) = Just assertion
getAssertion _ = Nothing


getErrorLogs :: Text -> [String]
getErrorLogs = mapMaybe errorFilter . parseLogs

errorFilter :: SbyLog -> Maybe String
errorFilter (Error error) = Just error
errorFilter _ = Nothing

parseLogs :: Text -> [SbyLog]
parseLogs logTxt =
    case runParser pSbyLog "" logTxt of
        Left  bundle -> error (errorBundlePretty bundle)
        Right l      -> l

pSbyLog :: TextParser [SbyLog]
pSbyLog =
    choice [
        M.some (lexeme pError),
        M.some (lexeme pSbyLogLine) ]

pSbyLogLine :: TextParser SbyLog
pSbyLogLine = do
    path <- pSbyHeader
    choice [
      try (SbyLogLine <$> pLogLine path),
      pAnyLine ]
    

pLogLine :: String -> TextParser LogLine
pLogLine path = do 
    choice [
        CoverLine     <$> pCover path,
        AssertionLine <$> pAssertion path ]

pCover :: String -> TextParser CoverLog
pCover path = do
    _ <- pEngineCover
    lexeme (
        choice [
            pCoverpointReached,
            pCoverpointUnreachd,
            pCoverpointVCD path,
            pCoverPointFail
        ] )

pEngineCover :: TextParser ()
pEngineCover = do
    _ <- pKeyword "engine_0: ##"
    _ <- pHour
    return ()

pCoverpointReached :: TextParser CoverLog
pCoverpointReached = do
    _ <- pKeyword "Reached cover statement at"
    coverPoint <- pProperty
    _ <- pKeyword "in step"
    step <- integer
    _ <- pKeyword "."
    return (CoverpointReached coverPoint step)

pCoverpointUnreachd :: TextParser CoverLog
pCoverpointUnreachd = do
    _ <- pKeyword "Unreached cover statement at"
    CoverpointUnreachd <$> pProperty

-- INSERIR PATH
pCoverpointVCD :: String -> TextParser CoverLog
pCoverpointVCD path = do
    CoverpointVCD <$> pWritingVCD

pCoverPointFail :: TextParser CoverLog
pCoverPointFail = do
    _ <- pKeyword "Assert failed in"
    entity <- pEntity
    _ <- pCharsc ':'
    property <- pProperty
    _ <- pCharsc '('
    _ <- pKeyword "step"
    step <- integer
    _ <- pCharsc ')'
    return (CoverPointFail entity property step)



pAssertion :: String -> TextParser AssertionLog
pAssertion path = do
    verifyType <- pEngineAssertion
    lexeme (
        choice [
            pAssertionStatus verifyType,
            pAssertionStep verifyType,
            pAssertionFail verifyType,
            pAssertionVCD verifyType path
        ] )

pEngineAssertion :: TextParser String
pEngineAssertion = do
    _ <- pKeyword "engine_0."
    verifyType <- T.unpack <$> pWord
    _ <- pKeyword ": ##"
    _ <- pHour
    return verifyType

pAssertionStatus :: String -> TextParser AssertionLog
pAssertionStatus verifyType = do
    AssertionStatus verifyType <$> pStatus

pAssertionStep :: String -> TextParser AssertionLog
pAssertionStep verifyType = do
    step <- pCheck "Checking assertions in step"
    return (AssertionStep verifyType step)

pAssertionFail :: String -> TextParser AssertionLog
pAssertionFail verifyType = do
    (entity, property) <- pAssertionFailed
    return (AssertionFail verifyType entity property)

--INSERIR PATH
pAssertionVCD :: String -> String -> TextParser AssertionLog
pAssertionVCD verifyType path = do
    AssertionVCD verifyType <$> pWritingVCD

pSbyHeader :: TextParser String
pSbyHeader = do
    _ <- pKeyword "SBY"
    _ <- pHour
    T.unpack <$> pBlock '[' ']' pPath

pError :: TextParser SbyLog
pError = do
    _ <- pKeyword "ERROR:"
    Error <$> (pAnything <* char '\n')

pAnyLine :: TextParser SbyLog
pAnyLine = AnyLine <$ lexeme pLine

pLine :: TextParser String
pLine = pAnything <* char '\n'