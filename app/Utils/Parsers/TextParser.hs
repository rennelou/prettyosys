{-# LANGUAGE OverloadedStrings #-}

module Utils.Parsers.TextParser (
    TextParser,
    lexeme,
    symbol,
    pCharsc,
    pWord,
    pKeyword,
    integer,
    pPath,
    pProcess,
    pHour,
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

integer :: TextParser Integer
integer = lexeme L.decimal

pPath :: TextParser Text
pPath = T.pack <$> lexeme (M.some pPathChar)

pPathChar :: TextParser Char
pPathChar = alphaNumChar <|> char '_' <|> char '-' <|> char '.' <|> char '/'

pProcess :: TextParser Text
pProcess = T.pack <$> lexeme (M.some (pProcessChar <|> char ' ' <|> char '%') )

pProcessChar :: TextParser Char
pProcessChar = pPathChar <|> char ';'

pHour :: TextParser (Integer, Integer, Integer)
pHour = do
    hour <- integer
    _ <- char ':'
    minute <- integer
    _ <- char ':' 
    seconds <- integer
    return (hour, minute, seconds)

pBlock :: Char -> Char -> TextParser a -> TextParser a
pBlock init end p = do
    void (pCharsc init)
    r <- p
    void (pCharsc end)
    return r
