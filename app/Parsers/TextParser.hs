module Parsers.TextParser (
    TextParser
) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)

type TextParser = Parsec Void Text