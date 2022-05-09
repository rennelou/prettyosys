{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.FileExtensions.Base (
    filterExtension,
    parserExtension,
    collapseAll
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

filterExtension :: TextParser String -> [String] -> [String]
filterExtension extensionParser = catMaybes.(map ((parseMaybe (extensionParser <* eof)).T.pack))

parserExtension :: String -> TextParser String
parserExtension extension = do
    name <- M.some (alphaNumChar <|> char '_' <|> char '/')
    vhdExtension <- T.unpack <$> string (T.pack extension)
    return (name ++ vhdExtension)

collapseAll :: TextParser String
collapseAll = do
    void (M.some alphaNumChar)
    return ""