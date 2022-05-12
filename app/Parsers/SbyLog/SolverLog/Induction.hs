{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog.Induction (
    Induction(..),
    pInduction
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

data Induction = 
    InductionSolver String deriving (Show)

pInduction :: TextParser Induction
pInduction = do
    _ <- pEngineInduction
    lexeme (
        choice [
            pInductionSolver
        ] )

pInductionSolver :: TextParser Induction
pInductionSolver = do
    _ <- pKeyword "Solver:"
    solver <- T.unpack <$> pWord
    return (InductionSolver solver)

pEngineInduction :: TextParser ()
pEngineInduction = do
    _ <- pKeyword "engine_0.induction: ##"
    _ <- pHour
    return ()