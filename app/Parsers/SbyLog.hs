{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog (
    pSbyLog
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

data SbyLogLine = SbyLogLine {
    taskPath    :: String,
    cmd         :: LogType
} deriving (Show)

data LogType =
      Moving   { movingFrom :: String, movingTo :: String }
    | Copy     { copingFrom :: String, copingTo :: String } 
    | CreateEngine String
    | SynthesisStart String
    | SynthesisFinished
    | SMT2DesignStart String
    | SMT2DesignFinished
    | StartEngine String deriving (Show)

pSbyLog :: TextParser [SbyLogLine]
pSbyLog = M.some (lexeme pSbyLogLine)

pSbyLogLine :: TextParser SbyLogLine
pSbyLogLine = do
    path <- pSbyHeader
    cmd <- pLogType
    return (SbyLogLine path cmd)

pLogType :: TextParser LogType
pLogType = lexeme (
    choice [
        pMoving,
        pCopy,
        try pSynthesisFinished,
        pSynthesisStart,
        pSMT2DesignStart,
        pSMT2DesignFinished,
        try pStartEngine,
        pCreateEngine
    ] )

pMoving :: TextParser LogType
pMoving = do
    _ <- pKeyword "Moving"
    _ <- pKeyword "directory"
    from <- T.unpack <$> pBlock '\'' '\'' pPath
    _ <- pKeyword "to"
    to <- T.unpack <$> pBlock '\'' '\'' pPath
    _ <- char '.'
    return (Moving from to)

pCopy :: TextParser LogType
pCopy = do
    _ <- pKeyword "Copy"
    from <- T.unpack <$> pBlock '\'' '\'' pPath
    _ <- pKeyword "to"
    to <- T.unpack <$> pBlock '\'' '\'' pPath
    _ <- char '.'
    return (Copy from to)

pCreateEngine :: TextParser LogType
pCreateEngine = do
    _ <- pEngine0
    engine <- T.unpack <$> pWord
    return (CreateEngine engine)

pSynthesisStart :: TextParser LogType
pSynthesisStart = do
    _ <- pBase
    _ <- pKeyword "starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (SynthesisStart process)

pSynthesisFinished :: TextParser LogType
pSynthesisFinished = SynthesisFinished <$ (pBase <* pKeyword "finished (returncode=0)")

pSMT2DesignStart :: TextParser LogType
pSMT2DesignStart = do
    _ <- pKeyword "smt2: starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (SMT2DesignStart process)

pSMT2DesignFinished :: TextParser LogType
pSMT2DesignFinished = SMT2DesignFinished <$ pKeyword "smt2: finished (returncode=0)"

pStartEngine :: TextParser LogType
pStartEngine = do
    _ <- pEngine0
    _ <- pKeyword "starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (StartEngine process)

pSbyHeader :: TextParser String
pSbyHeader = do
    _ <- pKeyword "SBY"
    hour <- integer
    _ <- char ':'
    minute <- integer
    _ <- char ':'
    seconds <- integer
    T.unpack <$> pBlock '[' ']' pPath

pEngine0 :: TextParser String
pEngine0 = do
    _ <- pKeyword "engine_0:"
    return "engine_0"

pBase :: TextParser String
pBase = do
    _ <- pKeyword "base:"
    return "base"