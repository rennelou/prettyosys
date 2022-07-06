{-# LANGUAGE OverloadedStrings #-}

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
    | InductionFailed
    | IndutionAssertFaild String String
    | InductionWritingVCD String
    | InductionWritingTestbench String
    | InductionWritingConstraints String
    | InductionStatus String deriving (Show)

pInduction :: TextParser Induction
pInduction = do
    _ <- pEngineInduction
    lexeme (
        choice [
            pInductionSolver,
            pInductionStep,
            pInductionSucess,
            pInductionFailed,
            pIndutionAssertFaild,
            pInductionWritingVCD,
            pInductionWritingTestbench,
            pInductionWritingConstraints,
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

pInductionFailed :: TextParser Induction
pInductionFailed = InductionFailed <$ pKeyword "Temporal induction failed!"

pIndutionAssertFaild :: TextParser Induction
pIndutionAssertFaild = do
    (entity, property) <- pAssertionFailed
    return (IndutionAssertFaild entity property)

pInductionWritingVCD :: TextParser Induction
pInductionWritingVCD = do
    InductionWritingVCD <$> pWritingVCD

pInductionWritingTestbench :: TextParser Induction
pInductionWritingTestbench = do
    InductionWritingTestbench <$> pWritingTestbench

pInductionWritingConstraints :: TextParser Induction
pInductionWritingConstraints = do
    InductionWritingConstraints <$> pWritingConstraint

pInductionStatus :: TextParser Induction
pInductionStatus = do
    InductionStatus <$> pStatus

pEngineInduction :: TextParser ()
pEngineInduction = do
    _ <- pKeyword "engine_0.induction: ##"
    _ <- pHour
    return ()