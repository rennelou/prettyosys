{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog.SolverLog (
    SolverLog(..),
    pSolverLog
) where

import Parsers.SbyLog.SolverLog.Cover
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

pSolverLog :: TextParser SolverLog
pSolverLog = choice [
        pCoverLog
    ]

pCoverLog :: TextParser SolverLog
pCoverLog = do
    cover <- pCover
    return (CoverLog cover)