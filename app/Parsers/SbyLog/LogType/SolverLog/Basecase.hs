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
    | FreeVarible String Integer
    | BasecaseFailed String String
    | BasecaseWritingVCD String 
    | BasecaseWritingTestbench String 
    | BasecaseWritingConstraints String 
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
            pFreeVariable,
            pBasecaseFailed,
            pBasecaseWritingVCD,
            pBasecaseWritingTestbench,
            pBasecaseWritingConstraints,
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

pFreeVariable :: TextParser Basecase
pFreeVariable = do
    _ <- pKeyword "Value for"
    freeBind <- T.unpack <$> pWord
    _ <- pKeyword "in"
    entity <- pWord
    _ <- pBlock '(' ')'
            ( do
                _ <- char '/'
                _ <- integer
                return () )
    _ <- pCharsc ':'
    value <- integer
    return (FreeVarible freeBind value)

pBasecaseFailed :: TextParser Basecase
pBasecaseFailed = do
    (entity, property) <- pAssertionFailed
    return (BasecaseFailed entity property)

pBasecaseWritingVCD :: TextParser Basecase
pBasecaseWritingVCD = do
    vcd <- pWritingVCD
    return (BasecaseWritingVCD vcd)

pBasecaseWritingTestbench :: TextParser Basecase
pBasecaseWritingTestbench = do
    testbench <- pWritingTestbench
    return (BasecaseWritingTestbench testbench)

pBasecaseWritingConstraints :: TextParser Basecase
pBasecaseWritingConstraints = do
    constraints <- pWritingConstraint
    return (BasecaseWritingConstraints constraints)

pBasecaseStatus :: TextParser Basecase
pBasecaseStatus = do
    status <- pStatus
    return (BasecaseStatus status)

pEngineBasecase :: TextParser ()
pEngineBasecase = do
    _ <- pKeyword "engine_0.basecase: ##"
    _ <- pHour
    return ()