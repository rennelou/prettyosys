{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog.Basecase (
    Basecase(..),
    pBasecase
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

data Basecase = 
    BasecaseSolver String deriving (Show)

pBasecase :: TextParser Basecase
pBasecase = do
    _ <- pEngineBasecase
    lexeme (
        choice [
            pBasecaseSolver
        ] )

pBasecaseSolver :: TextParser Basecase
pBasecaseSolver = do
    _ <- pKeyword "Solver:"
    solver <- T.unpack <$> pWord
    return (BasecaseSolver solver)

pEngineBasecase :: TextParser ()
pEngineBasecase = do
    _ <- pKeyword "engine_0.basecase: ##"
    _ <- pHour
    return ()