{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.LogType.SolverLog.Basecase (
    Basecase(..),
    pBasecase
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

data Basecase = 
      BasecaseSolver String 
    | AssumptionStep Integer
    | AssertionStep Integer
    | BMCFaild 
    | AssertionFailed String String
    | AssertionWritingVCD String 
    | AssertionWritingTestbench String 
    | AssertionWritingConstraints String 
    | BasecaseStatus String deriving (Show)

pBasecase :: TextParser Basecase
pBasecase = do
    _ <- pEngineBasecase
    lexeme (
        choice [
            pBasecaseSolver,
            pAssumptionStep,
            pAssertionStep,
            pBMCFaild,
            pAssertionFailed,
            pAssertionWritingVCD,
            pAssertionWritingTestbench,
            pAssertionWritingConstraints,
            pBasecaseStatus
        ] )

pBasecaseSolver :: TextParser Basecase
pBasecaseSolver = do
    _ <- pKeyword "Solver:"
    solver <- T.unpack <$> pWord
    return (BasecaseSolver solver)

pAssumptionStep :: TextParser Basecase
pAssumptionStep = do
    step <- pCheck "Checking assumptions in step"
    return (AssumptionStep step)

pAssertionStep :: TextParser Basecase
pAssertionStep = do
    step <- pCheck "Checking assertions in step"
    return (AssertionStep step)

pBMCFaild :: TextParser Basecase
pBMCFaild = BMCFaild <$ pKeyword "BMC failed!"

pAssertionFailed :: TextParser Basecase
pAssertionFailed = do
    _ <- pKeyword "Assert failed in"
    entity <- pEntity
    _ <- pCharsc ':'
    property <- pProperty
    return (AssertionFailed entity property)

pAssertionWritingVCD :: TextParser Basecase
pAssertionWritingVCD = do
    vcd <- pWritingVCD
    return (AssertionWritingVCD vcd)

pAssertionWritingTestbench :: TextParser Basecase
pAssertionWritingTestbench = do
    testbench <- pWritingTestbench
    return (AssertionWritingTestbench testbench)

pAssertionWritingConstraints :: TextParser Basecase
pAssertionWritingConstraints = do
    constraints <- pWritingConstraint
    return (AssertionWritingConstraints constraints)

pBasecaseStatus :: TextParser Basecase
pBasecaseStatus = do
    status <- pStatus
    return (BasecaseStatus status)

pEngineBasecase :: TextParser ()
pEngineBasecase = do
    _ <- pKeyword "engine_0.basecase: ##"
    _ <- pHour
    return ()