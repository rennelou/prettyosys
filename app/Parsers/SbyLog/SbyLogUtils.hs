{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SbyLogUtils (
    pEngine0,
    pBase
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

pEngine0 :: TextParser String
pEngine0 = do
    _ <- pKeyword "engine_0:"
    return "engine_0"

pBase :: TextParser String
pBase = do
    _ <- pKeyword "base:"
    return "base"