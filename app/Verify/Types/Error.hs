module Verify.Types.Error (
    getError
) where

import Parsers.SbyLog
import Data.List
import Text.Megaparsec hiding (State)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Verify.Types.Utils

getError :: FilePath -> T.Text -> String
getError workdir logs = intercalate "\n" $ getErrorLogs workdir logs