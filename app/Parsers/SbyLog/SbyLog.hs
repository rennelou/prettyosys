{-# LANGUAGE OverloadedStrings #-}

module Parsers.SbyLog.SbyLog (
    parseLogs,
    pSbyLog,
    pAssertion,
    SbyLog(..),
    LineWithContent(..),
    CoverLog(..),
    AssertionLog(..),
    VerifyType(..)
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
    SbyLogLine LineWithContent
  | AnyLine
  | Error String deriving (Show)

data LineWithContent = 
    CoverLine CoverLog
  | AssertionLine VerifyType AssertionLog deriving (Show)

data CoverLog = 
    CoverpointReached String Integer
  | CoverpointUnreachd String
  | CoverpointVCD String 
  | CoverPointFail String String Integer deriving (Show)

data AssertionLog =
    AssertionStatus String
  | AssertionStep Integer
  | AssertionFail String String
  | AssertionVCD String deriving (Show)

data VerifyType = 
    Basecase
  | Induction deriving (Eq, Show)

parseLogs :: String -> Text -> [SbyLog]
parseLogs workdir logTxt =
  case runParser (pSbyLog workdir) "" logTxt of
    Left  bundle -> error (errorBundlePretty bundle)
    Right l      -> l

pSbyLog :: String -> TextParser [SbyLog]
pSbyLog workdir =
  choice [
    M.some (lexeme pError),
    M.some (lexeme (pLogLine workdir)) ]

pLogLine :: String -> TextParser SbyLog
pLogLine workdir = do
  path <- pSbyHeader
  choice [
    try (SbyLogLine <$> pLineWithContent workdir path),
    pAnyLine ]

pSbyHeader :: TextParser String
pSbyHeader = do
  _ <- pKeyword "SBY"
  _ <- pHour
  T.unpack <$> pBlock '[' ']' pPath

pLineWithContent :: String -> String -> TextParser LineWithContent
pLineWithContent workdir path = do 
  choice [
    try (pCover     workdir path),
         pAssertion workdir path ]

pCover :: String -> String -> TextParser LineWithContent
pCover workdir path = do
  _ <- pEngineCover
  cover <- lexeme (
    choice [
      pCoverpointReached,
      pCoverpointUnreachd,
      pCoverpointVCD workdir path,
      pCoverPointFail
    ] )
  return (CoverLine cover)

pAssertion :: String -> String -> TextParser LineWithContent
pAssertion workdir path = do
  verifyType <- pEngineAssertion
  assertion <- lexeme (
    choice [
      pAssertionStatus,
      pAssertionStep,
      pAssertionFail,
      pAssertionVCD workdir path
    ] )
  return (AssertionLine verifyType assertion)



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
pCoverpointVCD :: String -> String -> TextParser CoverLog
pCoverpointVCD workdir path = do
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



pEngineAssertion :: TextParser VerifyType
pEngineAssertion = do
  _ <- string "engine_0."
  verifyType <- ( do
    choice [
      Basecase  <$ string "basecase",
      Induction <$ string "induction" ]
    )
  _ <- pKeyword ": ##"
  _ <- pHour
  return verifyType

pAssertionStatus :: TextParser AssertionLog
pAssertionStatus = do
  AssertionStatus <$> pStatus

pAssertionStep :: TextParser AssertionLog
pAssertionStep = do
  step <- pCheck "Checking assertions in step"
  return (AssertionStep step)

pAssertionFail :: TextParser AssertionLog
pAssertionFail = do
  (entity, property) <- pAssertionFailed
  return (AssertionFail entity property)

--INSERIR PATH
pAssertionVCD :: String -> String -> TextParser AssertionLog
pAssertionVCD workdir path = do
  AssertionVCD <$> pWritingVCD



pError :: TextParser SbyLog
pError = do
  _ <- pKeyword "ERROR:"
  Error <$> (pAnything <* char '\n')

pAnyLine :: TextParser SbyLog
pAnyLine = AnyLine <$ lexeme pLine

pLine :: TextParser String
pLine = pAnything <* char '\n'