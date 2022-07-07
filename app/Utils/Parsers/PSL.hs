{-# LANGUAGE OverloadedStrings #-}

module Utils.Parsers.PSL (
    PSLFile(..),
    getTopLevel,
    VunitType(..),
    pPSL
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

import Utils.Parsers.TextParser

data PSLFile = PSLFile {
    vunitType   :: VunitType,
    unitName    :: String,
    hdl         :: (String, String)
} deriving(Show)

data VunitType = VUnit | VProp | VMode deriving(Show)

getTopLevel :: PSLFile -> String
getTopLevel psl = fst $ hdl psl

pPSL :: TextParser PSLFile
pPSL = do
    vunitType <- pVunitType
    unitName  <- T.unpack <$> pWord
    hdl <- do
        void (pCharsc '(')
        entity <- T.unpack <$> pWord
        architecture <- T.unpack <$> between (pCharsc '(') (pCharsc ')') pWord
        void (pCharsc ')')
        return (entity, architecture)
    return (PSLFile vunitType unitName hdl)

pVunitType :: TextParser VunitType
pVunitType = choice
    [ VUnit <$ pKeyword "vunit"
    , VProp <$ pKeyword "vprop"
    , VMode <$ pKeyword "vmode" ]