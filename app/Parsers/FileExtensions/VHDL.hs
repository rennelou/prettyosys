module Parsers.FileExtensions.VHDL (
    filterVHDLExtension
) where

import Parsers.TextParser
import Parsers.FileExtensions.Base
import Text.Megaparsec hiding (State)

filterVHDLExtension :: [String] -> [String]
filterVHDLExtension = filterExtension parserVHDL

parserVHDL :: TextParser String
parserVHDL = try (parserExtension ".vhd") <|> collapseAll