{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog (
    SolverLog(..),
    pSolverLog
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

data SolverLog = CoverLog Cover deriving (Show)

data Cover = 
      SolverCover String
    | CoverStep Integer
    | ReachedCoverPoint String Integer deriving (Show)

pSolverLog :: TextParser SolverLog
pSolverLog = choice [
        pCoverLog
    ]

pCoverLog :: TextParser SolverLog
pCoverLog = do
    cover <- pCover
    return (CoverLog cover)

pCover :: TextParser Cover
pCover = do
    _ <- pEngineCover
    lexeme (
        choice [
            pSolverCover,
            pCoverStep,
            pReachedCoverPoint
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

pEngineCover :: TextParser String
pEngineCover = do
    _ <- pKeyword "engine_0: ##   0:00:00"
    return "Cover Engine"

pCoverPoint :: TextParser String
pCoverPoint = lexeme (M.some (alphaNumChar <|> char '_' <|> char '.') )
