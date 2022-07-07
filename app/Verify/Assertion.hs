{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Verify.Assertion (
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data Assertion = Assertion {
    _ATpassed           :: Bool,
    _ATstep             :: Integer,
    _ATassertionFailed  :: Maybe String,
    _ATtrace            :: Maybe String
} deriving (Show)

getCoverAssertion :: FilePath -> BL.ByteString -> [(String, Assertion)]
getCoverAssertion workdir =
    insertTag "Cover" . mapCoverToAssertion . getCoverLogs workdir . T.decodeUtf8 . B.concat . BL.toChunks

mapCoverToAssertion :: [CoverLog] -> [Assertion]
mapCoverToAssertion = map toAssertion . getValidCoverEvents
  where
      toAssertion :: CoverLog -> Assertion
      toAssertion (CoverPointFail entity name step) =
        Assertion False step (Just name) Nothing

      toAssertion _ = error "Error converting Cover to Assertion"

      getValidCoverEvents :: [CoverLog] -> [CoverLog]
      getValidCoverEvents = filter isCoverValid
        where
          isCoverValid :: CoverLog -> Bool
          isCoverValid CoverPointFail {} = True
          isCoverValid _ = False

getBasecaseAssertion :: FilePath -> BL.ByteString -> [(String, Assertion)]
getBasecaseAssertion workdir =
    insertTag (show Basecase) . toAssertionSingleton. mapAssertionLogsToAssertion Basecase . getAssertionLogs workdir . T.decodeUtf8 . B.concat . BL.toChunks

getInductionAssertion :: FilePath -> BL.ByteString -> [(String, Assertion)]
getInductionAssertion workdir =
    insertTag (show Induction) . toAssertionSingleton . mapAssertionLogsToAssertion Induction . getAssertionLogs workdir . T.decodeUtf8 . B.concat . BL.toChunks

mapAssertionLogsToAssertion :: VerifyType -> [(VerifyType, AssertionLog)] -> Assertion
mapAssertionLogsToAssertion verifyType = mapToAssertion . filterByVerifyType verifyType

mapToAssertion :: [AssertionLog] -> Assertion
mapToAssertion = foldl nextState (Assertion False 0 Nothing Nothing)

filterByVerifyType :: VerifyType -> [(VerifyType, AssertionLog)] -> [AssertionLog]
filterByVerifyType verifyType = map snd . (filter (\ (_verifyType, _) -> _verifyType == verifyType))

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