module Parsers.FileExtensions.PSL (
    filterPSLExtension
) where

import Parsers.TextParser
import Parsers.FileExtensions.Base
import Text.Megaparsec hiding (State)
import Parsers.FileExtensions.VHDL

filterPSLExtension :: [String] -> [String]
filterPSLExtension = filterExtension parserPSL

parserPSL :: TextParser String
parserPSL = try (parserExtension ".psl")
           <|> try parserVHDL
           <|> collapseAll