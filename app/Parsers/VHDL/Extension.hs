{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.VHDL.Extension (
    filterVHDLExtension
) where

import Control.Applicative
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

filterVHDLExtension :: [String] -> [String]
filterVHDLExtension = (catMaybes . (map ((parseMaybe (parseVHDLOrEmpty <* eof)) . T.pack)))

parseVHDLOrEmpty :: TextParser String
parseVHDLOrEmpty = try parserVHDL <|> collapseNotVHDL

parserVHDL :: TextParser String
parserVHDL = do
    name <- M.some (alphaNumChar <|> char '_' <|> char '/')
    vhdExtension <- T.unpack <$> string ".vhd"
    return (name ++ vhdExtension)

collapseNotVHDL :: TextParser String
collapseNotVHDL = do
    void (M.some alphaNumChar)
    return ""