{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog.Cover (
    Cover(..),
    pCover
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
    _ <- pKeyword "Checking cover reachability in step"
    step <- integer
    _ <- pKeyword ".."
    return (CoverStep step)

pReachedCoverPoint :: TextParser Cover
pReachedCoverPoint = do
    _ <- pKeyword "Reached cover statement at"
    coverPoint <- pCoverPoint
    _ <- pKeyword "in step"
    step <- integer
    _ <- pKeyword "."
    return (ReachedCoverPoint coverPoint step)

pUnreachdCoverPoint :: TextParser Cover
pUnreachdCoverPoint = do
    _ <- pKeyword "Unreached cover statement at"
    coverPoint <- pCoverPoint
    return (UnreachdCoverPoint coverPoint)

pWritingCoverVCD :: TextParser Cover
pWritingCoverVCD = do
    _ <- pKeyword "Writing trace to VCD file:"
    trace <- T.unpack <$> pPath
    return (WritingCoverVCD trace)

pWritingCoverTestbench :: TextParser Cover
pWritingCoverTestbench = do
    _ <- pKeyword "Writing trace to Verilog testbench:"
    testbench <- T.unpack <$> pPath
    return (WritingCoverTestbench testbench)

pWritingCoverConstraint :: TextParser Cover
pWritingCoverConstraint = do
    _ <- pKeyword "Writing trace to constraints file:"
    constraints <- T.unpack <$> pPath
    return (WritingCoverConstraint constraints)

pCoverStatus :: TextParser Cover
pCoverStatus = do
    _ <- pKeyword "Status:"
    status <- T.unpack <$> pWord
    return (CoverStatus status)

pEngineCover :: TextParser String
pEngineCover = do
    _ <- pKeyword "engine_0: ##"
    _ <- pHour
    return "Cover Engine"

pCoverPoint :: TextParser String
pCoverPoint = lexeme (M.some (alphaNumChar <|> char '_' <|> char '.' <|> char ':') )