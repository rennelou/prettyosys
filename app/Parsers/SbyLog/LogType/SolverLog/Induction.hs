{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.LogType.SolverLog.Induction (
    Induction(..),
    pInduction
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

data Induction = 
      InductionSolver String
    | InductionStep Integer 
    | InductionSucess 
    | InductionStatus String deriving (Show)

pInduction :: TextParser Induction
pInduction = do
    _ <- pEngineInduction
    lexeme (
        choice [
            pInductionSolver,
            pInductionStep,
            pInductionSucess,
            pInductionStatus
        ] )

pInductionSolver :: TextParser Induction
pInductionSolver = do
    _ <- pKeyword "Solver:"
    solver <- T.unpack <$> pWord
    return (InductionSolver solver)

pInductionStep :: TextParser Induction
pInductionStep = do
    step <- pCheck "Trying induction in step"
    return (InductionStep step)

pInductionSucess :: TextParser Induction
pInductionSucess = InductionSucess <$ pKeyword "Temporal induction successful."

pInductionStatus :: TextParser Induction
pInductionStatus = do
    status <- pStatus
    return (InductionStatus status)

pEngineInduction :: TextParser ()
pEngineInduction = do
    _ <- pKeyword "engine_0.induction: ##"
    _ <- pHour
    return ()