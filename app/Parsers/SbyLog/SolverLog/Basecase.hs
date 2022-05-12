{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog.Basecase (
    Basecase(..),
    pBasecase
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

data Basecase = 
      BasecaseSolver String 
    | AssumptionStep Integer
    | AssertionStep Integer
    | BMCFaild 
    | AssertionFailed String String deriving (Show)

pBasecase :: TextParser Basecase
pBasecase = do
    _ <- pEngineBasecase
    lexeme (
        choice [
            pBasecaseSolver,
            pAssumptionStep,
            pAssertionStep,
            pBMCFaild,
            pAssertionFailed
        ] )

pBasecaseSolver :: TextParser Basecase
pBasecaseSolver = do
    _ <- pKeyword "Solver:"
    solver <- T.unpack <$> pWord
    return (BasecaseSolver solver)

pAssumptionStep :: TextParser Basecase
pAssumptionStep = do
    _ <- pKeyword "Checking assumptions in step"
    step <- integer
    _ <- pKeyword ".."
    return (AssumptionStep step)

pAssertionStep :: TextParser Basecase
pAssertionStep = do
    _ <- pKeyword "Checking assertions in step"
    step <- integer
    _ <- pKeyword ".."
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

pEngineBasecase :: TextParser ()
pEngineBasecase = do
    _ <- pKeyword "engine_0.basecase: ##"
    _ <- pHour
    return ()