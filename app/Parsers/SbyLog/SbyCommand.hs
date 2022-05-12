{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SbyCommand (
    SbyCommand(..),
    pSbyCommand
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

data SbyCommand =
      Moving   { movingFrom :: String, movingTo :: String }
    | Copy     { copingFrom :: String, copingTo :: String } 
    | CreateEngine String
    | SynthesisStart String
    | SynthesisFinished
    | SMT2DesignStart String
    | SMT2DesignFinished
    | StartEngine String
    | FinishedEngine Integer
    | EngineStatus String
    | Summury 
    | Done deriving (Show)

pSbyCommand :: TextParser SbyCommand
pSbyCommand = 
    lexeme (
        choice [
            pMoving,
            pCopy,
            pSynthesisStart,
            pSynthesisFinished,
            pSMT2DesignStart,
            pSMT2DesignFinished,
            pStartEngine,
            pFinishedEngine,
            pEngineStatus,
            pSummury,
            pDone,
            pCreateEngine
        ] )

pMoving :: TextParser SbyCommand
pMoving = do
    _ <- pKeyword "Moving"
    _ <- pKeyword "directory"
    from <- T.unpack <$> pBlock '\'' '\'' pPath
    _ <- pKeyword "to"
    to <- T.unpack <$> pBlock '\'' '\'' pPath
    _ <- char '.'
    return (Moving from to)

pCopy :: TextParser SbyCommand
pCopy = do
    _ <- pKeyword "Copy"
    from <- T.unpack <$> pBlock '\'' '\'' pPath
    _ <- pKeyword "to"
    to <- T.unpack <$> pBlock '\'' '\'' pPath
    _ <- char '.'
    return (Copy from to)

pCreateEngine :: TextParser SbyCommand
pCreateEngine = do
    _ <- pEngine0
    engine <- ((M.some alphaNumChar) <* (string "\n"))
    return (CreateEngine engine)

pSynthesisStart :: TextParser SbyCommand
pSynthesisStart = do
    _ <- pKeyword "base: starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (SynthesisStart process)

pSynthesisFinished :: TextParser SbyCommand
pSynthesisFinished = SynthesisFinished <$ pKeyword "base: finished (returncode=0)"

pSMT2DesignStart :: TextParser SbyCommand
pSMT2DesignStart = do
    _ <- pKeyword "smt2: starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (SMT2DesignStart process)

pSMT2DesignFinished :: TextParser SbyCommand
pSMT2DesignFinished = SMT2DesignFinished <$ pKeyword "smt2: finished (returncode=0)"

pStartEngine :: TextParser SbyCommand
pStartEngine = do
    _ <- pKeyword "engine_0: starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (StartEngine process)

pFinishedEngine :: TextParser SbyCommand
pFinishedEngine = do
    _ <- string "engine_0: finished (returncode="
    returnCode <- integer
    _ <- pCharsc ')'
    return (FinishedEngine returnCode)

pEngineStatus :: TextParser SbyCommand
pEngineStatus = do
    _ <- pKeyword "engine_0: Status returned by engine:"
    status <- T.unpack <$> pWord
    return (EngineStatus status)

pSummury :: TextParser SbyCommand
pSummury = Summury <$ (pKeyword "summary:" <* pAnything <* char '\n')

pDone :: TextParser SbyCommand
pDone = Done <$ (pKeyword "DONE" <* pAnything <* char '\n')

pEngine0 :: TextParser String
pEngine0 = do
    _ <- pKeyword "engine_0:"
    return "engine_0"

pAnything :: TextParser String
pAnything = M.many (
        alphaNumChar
    <|> char ' '
    <|> char '(' 
    <|> char ')' 
    <|> char '['
    <|> char ']'
    <|> char ':'
    <|> char ','
    <|> char '_'
    <|> char '=' )