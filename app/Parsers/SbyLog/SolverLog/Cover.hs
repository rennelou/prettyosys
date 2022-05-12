{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog.Cover (
    Cover(..),
    pCover
) where

import Parsers.SbyLog.SolverLog.Utils

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
    coverPoint <- pProperty
    return (UnreachdCoverPoint coverPoint)

pWritingCoverVCD :: TextParser Cover
pWritingCoverVCD = do
    trace <- pWritingVCD
    return (WritingCoverVCD trace)

pWritingCoverTestbench :: TextParser Cover
pWritingCoverTestbench = do
    testbench <- pWritingTestbench
    return (WritingCoverTestbench testbench)

pWritingCoverConstraint :: TextParser Cover
pWritingCoverConstraint = do
    constraints <- pWritingConstraint
    return (WritingCoverConstraint constraints)

pCoverStatus :: TextParser Cover
pCoverStatus = do
    status <- pStatus
    return (CoverStatus status)

pEngineCover :: TextParser ()
pEngineCover = do
    _ <- pKeyword "engine_0: ##"
    _ <- pHour
    return ()