{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Verify.Types.Utils (
  getCoverLogs,
  getAssertionLogs,
  getErrorLogs
) where
import qualified Data.Text as T
import Data.Maybe

import Parsers.SbyLog.SbyLog

getCoverLogs :: FilePath -> T.Text -> [CoverLog]
getCoverLogs workdir =
  mapMaybe (\ coverLog ->
    case coverLog of
      (SbyLogLine (CoverLine cover)) -> Just cover
      _                              -> Nothing ) . parseLogs workdir

getAssertionLogs :: FilePath -> T.Text -> [(VerifyType, AssertionLog)]
getAssertionLogs workdir =
  mapMaybe (\ assertionLog ->
    case assertionLog of
      (SbyLogLine (AssertionLine verifyType assertion)) -> Just (verifyType, assertion)
      _                                                 -> Nothing) . parseLogs workdir

getErrorLogs :: FilePath -> T.Text -> [String]
getErrorLogs workdir =
  mapMaybe (\ errorLog ->
    case errorLog of
      (Error error) -> Just error
      _             -> Nothing) . parseLogs workdir