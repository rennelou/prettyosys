{-# LANGUAGE OverloadedStrings #-}

module Utils.Parsers.PSL (
    PSL(..),
    VunitType(..),
    tryParsePsl,
    pPSL,
    getTopLevel
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

data PSL = PSL {
    vunitType   :: VunitType,
    unitName    :: String,
    hdl         :: (String, String)
} deriving(Show)

data VunitType = VUnit | VProp | VMode deriving(Show)

getTopLevel :: PSL -> String
getTopLevel psl = fst $ hdl psl

tryParsePsl :: T.Text -> Maybe PSL
tryParsePsl s =
  case runParser pPSL "" s of
    Left  err -> Nothing
    Right psl -> Just psl

pPSL :: TextParser PSL
pPSL = do
  vunitType <- pVunitType
  unitName  <- T.unpack <$> pWord
  hdl <- do
    void (pCharsc '(')
    entity <- T.unpack <$> pWord
    architecture <- T.unpack <$> between (pCharsc '(') (pCharsc ')') pWord
    void (pCharsc ')')
    return (entity, architecture)
  return (PSL vunitType unitName hdl)

pVunitType :: TextParser VunitType
pVunitType = choice
  [ VUnit <$ pKeyword "vunit"
  , VProp <$ pKeyword "vprop"
  , VMode <$ pKeyword "vmode" ]