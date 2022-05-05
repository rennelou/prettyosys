module Parsers.FileExtensions.VHDL (
    filterVHDLExtension,
    parserVHDL
) where

import Parsers.TextParser
import Parsers.FileExtensions.Base
import Text.Megaparsec hiding (State)

filterVHDLExtension :: [String] -> [String]
filterVHDLExtension = filterExtension parserVHDL

parserVHDL :: TextParser String
parserVHDL = try (parserExtension ".vhd") 
             <|> try (parserExtension ".vhdl") 
             <|> collapseAll