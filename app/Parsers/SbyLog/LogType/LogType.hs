{-# LANGUAGE OverloadedStrings #-}

module Parsers.SbyLog.LogType.LogType (
    LogType(..),
    pLogType,
    SolverLog(..),
    Cover(..),
    Basecase(..),
    Induction(..)
) where

import Parsers.SbyLog.Utils
import Parsers.SbyLog.LogType.SolverLog.SolverLog
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

data LogType = SbyCommand | SolverType SolverLog deriving (Show)

pLogType :: TextParser LogType
pLogType = choice [
        pSolverType,
        pSbyCommand
    ]

pSolverType :: TextParser LogType
pSolverType = do
    SolverType <$> pSolverLog

pSbyCommand :: TextParser LogType
pSbyCommand = lexeme pLine

pLine :: TextParser LogType
pLine = SbyCommand <$ (pAnything <* char '\n')