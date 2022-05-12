{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SolverLog.Utils (
    pEntity,
    pProperty
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

pEntity :: TextParser String
pEntity = lexeme (M.some (alphaNumChar <|> char '_') )

pProperty :: TextParser String
pProperty = lexeme (M.some (alphaNumChar <|> char '_' <|> char '.' <|> char ':') )