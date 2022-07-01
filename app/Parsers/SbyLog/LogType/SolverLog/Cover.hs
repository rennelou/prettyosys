{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.LogType.SolverLog.Cover (
    Cover(..),
    pCover
) where

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

data Cover =
      SolverCover String
    | CoverStep Integer
    | ReachedCoverPoint String Integer
    | UnreachdCoverPoint String
    | CoverAssertFailed String String Integer
    | WaitingSolver Integer
    | WritingCoverVCD String
    | WritingCoverTestbench String
    | WritingCoverConstraint String
    | CoverStatus String deriving (Show)

pCover :: TextParser Cover
pCover = do
    _ <- pEngineCover
    lexeme (
        choice [
            pSolverCover,
            pCoverStep,
            pReachedCoverPoint,
            pUnreachdCoverPoint,
            pCoverAssertFailed,
            pWaitingSolver,
            pWritingCoverVCD,
            pWritingCoverTestbench,
            pWritingCoverConstraint,
            pCoverStatus
        ] )

pSolverCover :: TextParser Cover
pSolverCover = do
    _ <- pKeyword "Solver:"
    solver <- T.unpack <$> pWord
    return (SolverCover solver)

pCoverStep :: TextParser Cover
pCoverStep = do
    step <- pCheck "Checking cover reachability in step"
    return (CoverStep step)

pReachedCoverPoint :: TextParser Cover
pReachedCoverPoint = do
    _ <- pKeyword "Reached cover statement at"
    coverPoint <- pProperty
    _ <- pKeyword "in step"
    step <- integer
    _ <- pKeyword "."
    return (ReachedCoverPoint coverPoint step)

pUnreachdCoverPoint :: TextParser Cover
pUnreachdCoverPoint = do
    _ <- pKeyword "Unreached cover statement at"
    UnreachdCoverPoint <$> pProperty

pCoverAssertFailed :: TextParser Cover
pCoverAssertFailed = do
    _ <- pKeyword "Assert failed in"
    entity <- pEntity
    _ <- pCharsc ':'
    property <- pProperty
    _ <- pCharsc '('
    _ <- pKeyword "step"
    step <- integer
    _ <- pCharsc ')'
    return (CoverAssertFailed entity property step)

pWaitingSolver :: TextParser Cover
pWaitingSolver = do
    _ <- pKeyword "waiting for solver"
    _ <- pCharsc '('
    time <- integer
    _ <- pKeyword "minute"
    _ <- pCharsc ')'
    return (WaitingSolver time)


pWritingCoverVCD :: TextParser Cover
pWritingCoverVCD = do
    WritingCoverVCD <$> pWritingVCD

pWritingCoverTestbench :: TextParser Cover
pWritingCoverTestbench = do
    WritingCoverTestbench <$> pWritingTestbench

pWritingCoverConstraint :: TextParser Cover
pWritingCoverConstraint = do
    WritingCoverConstraint <$> pWritingConstraint

pCoverStatus :: TextParser Cover
pCoverStatus = do
    CoverStatus <$> pStatus

pEngineCover :: TextParser ()
pEngineCover = do
    _ <- pKeyword "engine_0: ##"
    _ <- pHour
    return ()