{-# LANGUAGE OverloadedStrings #-}

module Parsers.SbyLog.Utils (
    pEntity,
    pProperty,
    pCheck,
    pAssertionFailed,
    pWritingVCD,
    pWritingTestbench,
    pWritingConstraint,
    pStatus,
    pAnything
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
pProperty = lexeme (M.some (alphaNumChar <|> char '_' <|> char '.' <|> char ':' <|> char '/') )

pCheck :: String -> TextParser Integer
pCheck checkType = do
    _ <- pKeyword $ T.pack checkType
    step <- integer
    _ <- pKeyword ".."
    return step

pAssertionFailed :: TextParser (String, String)
pAssertionFailed = do
    _ <- pKeyword "Assert failed in"
    entity <- pEntity
    _ <- pCharsc ':'
    property <- pProperty
    return (entity, property)

pWritingVCD :: TextParser String
pWritingVCD = do
    _ <- pKeyword "Writing trace to VCD file:"
    T.unpack <$> pPath

pWritingTestbench :: TextParser String
pWritingTestbench = do
    _ <- pKeyword "Writing trace to Verilog testbench:"
    T.unpack <$> pPath

pWritingConstraint :: TextParser String
pWritingConstraint = do
    _ <- pKeyword "Writing trace to constraints file:"
    T.unpack <$> pPath

pStatus :: TextParser String
pStatus = do
    _ <- pKeyword "Status:"
    T.unpack <$> pWord

pAnything :: TextParser String
pAnything = M.many (satisfy (/= '\n'))