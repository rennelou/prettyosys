module Verify.Error (
    getError
) where

import Parsers.SbyLog.SbyLog
import Data.List
import Text.Megaparsec hiding (State)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

getError :: BL.ByteString -> String
getError = (intercalate "\n") . getErrorLogs . (T.decodeUtf8 . B.concat . BL.toChunks)