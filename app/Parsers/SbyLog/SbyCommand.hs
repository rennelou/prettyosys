{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.SbyLog.SbyCommand (
    SbyCommand(..),
    pSbyCommand
) where

import Parsers.SbyLog.SbyLogUtils
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
    | StartEngine String deriving (Show)

pSbyCommand :: TextParser SbyCommand
pSbyCommand = 
    lexeme (
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
    engine <- T.unpack <$> pWord
    return (CreateEngine engine)

pSynthesisStart :: TextParser SbyCommand
pSynthesisStart = do
    _ <- pBase
    _ <- pKeyword "starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (SynthesisStart process)

pSynthesisFinished :: TextParser SbyCommand
pSynthesisFinished = SynthesisFinished <$ (pBase <* pKeyword "finished (returncode=0)")

pSMT2DesignStart :: TextParser SbyCommand
pSMT2DesignStart = do
    _ <- pKeyword "smt2: starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (SMT2DesignStart process)

pSMT2DesignFinished :: TextParser SbyCommand
pSMT2DesignFinished = SMT2DesignFinished <$ pKeyword "smt2: finished (returncode=0)"

pStartEngine :: TextParser SbyCommand
pStartEngine = do
    _ <- pEngine0
    _ <- pKeyword "starting process"
    process <- T.unpack <$> pBlock '\"' '\"' pProcess
    return (StartEngine process)