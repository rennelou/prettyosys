{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog.SolverLog (
    SolverLog(..),
    Cover(..),
    Basecase(..),
    Induction(..),
    pSolverLog
) where

import Parsers.SbyLog.SolverLog.Cover
import Parsers.SbyLog.SolverLog.Basecase
import Parsers.SbyLog.SolverLog.Induction
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

data SolverLog = CoverLog Cover | BasecaseLog Basecase | InductionLog Induction deriving (Show)

pSolverLog :: TextParser SolverLog
pSolverLog = choice [
        pCoverLog,
        pBasecaseLog,
        pInductionLog
    ]

pCoverLog :: TextParser SolverLog
pCoverLog = do
    cover <- pCover
    return (CoverLog cover)

pBasecaseLog :: TextParser SolverLog
pBasecaseLog = do
    basecase <- pBasecase
    return (BasecaseLog basecase)

pInductionLog :: TextParser SolverLog
pInductionLog = do
    induction <- pInduction
    return (InductionLog induction)