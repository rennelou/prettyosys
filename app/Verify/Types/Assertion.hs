{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda-case" #-}

module Verify.Types.Assertion (
    Assertion(..),
    getCoverAssertion,
    getBasecaseAssertion,
    getInductionAssertion
) where

import System.Directory

import Parsers.SbyLog.SbyLog
import Text.Megaparsec hiding (State)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Maybe
import RTL.RTL (RTLArgs(workdir))

data Assertion = Assertion {
    _ATpassed           :: Bool,
    _ATstep             :: Integer,
    _ATassertionFailed  :: Maybe String,
    _ATtrace            :: Maybe String
} deriving (Show)

getCoverAssertion :: FilePath -> T.Text -> [(String, Assertion)]
getCoverAssertion workdir logs = insertTag "Cover" . mapCoverToAssertion $ (getCoverLogs workdir logs)

mapCoverToAssertion :: [CoverLog] -> [Assertion]
mapCoverToAssertion =
  mapMaybe 
    (\ cover ->
      case cover of
        CoverPointFail entity name step -> Just (Assertion False step (Just name) Nothing)
        _                               -> Nothing )



getBasecaseAssertion :: FilePath -> T.Text -> [(String, Assertion)]
getBasecaseAssertion workdir logs =
  insertTag (show Basecase) . toAssertionSingleton. mapAssertionLogsToAssertion Basecase $ (getAssertionLogs workdir logs)

getInductionAssertion :: FilePath -> T.Text -> [(String, Assertion)]
getInductionAssertion workdir logs =
  insertTag (show Induction) . toAssertionSingleton . mapAssertionLogsToAssertion Induction $ (getAssertionLogs workdir logs)



mapAssertionLogsToAssertion :: VerifyType -> [(VerifyType, AssertionLog)] -> Assertion
mapAssertionLogsToAssertion verifyType = 
  transformLogsInAssertions . getLogIfVerifyTypeMatch verifyType

transformLogsInAssertions :: [AssertionLog] -> Assertion
transformLogsInAssertions = foldl nextState (Assertion False 0 Nothing Nothing)

getLogIfVerifyTypeMatch :: VerifyType -> [(VerifyType, AssertionLog)] -> [AssertionLog]
getLogIfVerifyTypeMatch verifyType = 
  mapMaybe
    (\ (_verifyType, assertionLog) ->
      if _verifyType == verifyType then
        Just assertionLog
      else
        Nothing )

nextState :: Assertion -> AssertionLog -> Assertion
nextState assertion@(Assertion _ step name trace) (AssertionStatus passed) =
      Assertion (statusToBool passed) step name trace
nextState assertion@(Assertion passed _ name trace) (AssertionStep step) =
      Assertion passed step name trace
nextState assertion@(Assertion passed step _ trace) (AssertionFail _ name) =
      Assertion passed step (Just name) trace
nextState assertion@(Assertion passed step name _) (AssertionVCD trace) =
      Assertion passed step name (Just trace)

insertTag :: String -> [Assertion] -> [(String, Assertion)]
insertTag tag = map (\ a -> (tag, a))

toAssertionSingleton :: Assertion -> [Assertion]
toAssertionSingleton (Assertion False 0 Nothing Nothing) = []
toAssertionSingleton a = [a]

statusToBool :: String -> Bool
statusToBool "passed" = True
statusToBool _ = False



getCoverLogs :: FilePath -> T.Text -> [CoverLog]
getCoverLogs currentDirectory = mapMaybe getCover . parseLogs
    
getCover :: SbyLog -> Maybe CoverLog
getCover (SbyLogLine (CoverLine cover)) = Just cover
getCover _ = Nothing

getAssertionLogs :: FilePath -> T.Text -> [(VerifyType, AssertionLog)]
getAssertionLogs currentDirectory = mapMaybe getAssertion . parseLogs

getAssertion :: SbyLog -> Maybe (VerifyType, AssertionLog)
getAssertion (SbyLogLine (AssertionLine verifyType assertion)) = Just (verifyType, assertion)
getAssertion _ = Nothing