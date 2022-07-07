module Verify.Types.Error (
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
import Data.Maybe

getError :: T.Text -> String
getError logs = intercalate "\n" $ getErrorLogs logs

getErrorLogs :: T.Text -> [String]
getErrorLogs = mapMaybe errorFilter . parseLogs

errorFilter :: SbyLog -> Maybe String
errorFilter (Error error) = Just error
errorFilter _ = Nothing