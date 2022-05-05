module Parsers.FileExtensions.VU (
    filterVUExtension
) where

import Parsers.TextParser
import Parsers.FileExtensions.Base
import Text.Megaparsec hiding (State)
import Parsers.FileExtensions.VHDL

filterVUExtension :: [String] -> [String]
filterVUExtension = filterExtension parserVU

parserVU :: TextParser String
parserVU = try parserVHDL
           <|> collapseAll