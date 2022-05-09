{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.TextParser (
    TextParser,
    lexeme,
    symbol,
    pCharsc,
    pWord,
    pKeyword,
    pBlock
) where

import Control.Monad
import Control.Applicative hiding (some)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type TextParser = Parsec Void Text

lexeme :: TextParser a -> TextParser a
lexeme = L.lexeme sc

symbol :: Text -> TextParser Text
symbol = L.symbol sc

sc :: TextParser ()
sc = L.space 
        (void $ some (char ' ' <|> char '\t' <|> char '\r' <|> char '\n'))
        (L.skipLineComment "--")
        empty

pCharsc :: Char -> TextParser Char
pCharsc c = lexeme (char c)

pWord :: TextParser Text
pWord = T.pack <$> lexeme (M.some (alphaNumChar <|> char '_') )

pKeyword :: Text -> TextParser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

pBlock :: Char -> Char -> TextParser a -> TextParser a
pBlock init end p = do
    void (pCharsc init)
    r <- p
    void (pCharsc end)
    return r