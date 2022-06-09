{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
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

mapCoverToAssertion :: [Cover] -> [Assertion]
mapCoverToAssertion = map toAssertion . getValidCoverEvents
  where
      toAssertion :: Cover -> Assertion
      toAssertion (CoverAssertFailed entity name step) =
        Assertion False step (Just name) Nothing

      toAssertion _ = error "Error converting Cover to Assertion"

      getValidCoverEvents :: [Cover] -> [Cover]
      getValidCoverEvents = filter isCoverValid
        where
          isCoverValid :: Cover -> Bool
          isCoverValid CoverAssertFailed {} = True
          isCoverValid _ = False

getBasecaseAssertion :: FilePath -> BL.ByteString -> [(String, Assertion)]
getBasecaseAssertion workdir =
    insertTag "Basecase" . toAssertionSingleton. mapBasecaseToAssertion . getBasecaseLogs workdir . T.decodeUtf8 . B.concat . BL.toChunks

getInductionAssertion :: FilePath -> BL.ByteString -> [(String, Assertion)]
getInductionAssertion workdir =
    insertTag "Induction" . toAssertionSingleton . mapInductionToAssertion . getInductionLogs workdir . T.decodeUtf8 . B.concat . BL.toChunks

mapBasecaseToAssertion :: [Basecase] -> Assertion
mapBasecaseToAssertion = mapToAssertion nextState . getValidBasecaseEvents
    where
        nextState :: Assertion -> Basecase -> Assertion
        nextState (Assertion _ step name trace) (BasecaseStatus passed) =
            Assertion (statusToBool passed) step name trace

        nextState (Assertion passed _ name trace) (AssertionStep step) =
            Assertion passed step name trace

        nextState (Assertion passed step _ trace) (BasecaseFailed _ name) =
            Assertion passed step (Just name) trace

        nextState (Assertion passed step name _) (BasecaseWritingVCD trace) =
            Assertion passed step name (Just trace)

        nextState _ _ = error "error creating assertion"

        getValidBasecaseEvents :: [Basecase] -> [Basecase]
        getValidBasecaseEvents = filter isBasecaseValid
            where
                isBasecaseValid :: Basecase -> Bool
                isBasecaseValid (BasecaseStatus _) = True
                isBasecaseValid (AssertionStep _) = True
                isBasecaseValid (BasecaseFailed _ _) = True
                isBasecaseValid (BasecaseWritingVCD _) = True
                isBasecaseValid _ = False

mapInductionToAssertion :: [Induction] -> Assertion
mapInductionToAssertion = mapToAssertion nextState . getValidInductionEvents
    where
        nextState :: Assertion -> Induction -> Assertion
        nextState (Assertion _ step name trace) (InductionStatus passed) =
            Assertion (statusToBool passed) step name trace

        nextState (Assertion passed _ name trace) (InductionStep step) =
            Assertion passed step name trace

        nextState (Assertion passed step _ trace) (IndutionAssertFaild _ name) =
            Assertion passed step (Just name) trace

        nextState (Assertion passed step name _) (InductionWritingVCD trace) =
            Assertion passed step name (Just trace)

        nextState _ _ = error "error creating assertion"

        getValidInductionEvents :: [Induction] -> [Induction]
        getValidInductionEvents = filter isInductionValid
            where
                isInductionValid :: Induction -> Bool
                isInductionValid (InductionStatus _) = True
                isInductionValid (InductionStep _) = True
                isInductionValid (IndutionAssertFaild _ _) = True
                isInductionValid (InductionWritingVCD _) = True
                isInductionValid _ = False

insertTag :: String -> [Assertion] -> [(String, Assertion)]
insertTag tag = map (\ a -> (tag, a))

toAssertionSingleton :: Assertion -> [Assertion]
toAssertionSingleton (Assertion False 0 Nothing Nothing) = []
toAssertionSingleton a = [a]

mapToAssertion :: (Assertion -> a -> Assertion) -> [a] -> Assertion
mapToAssertion nextState = foldl nextState (Assertion False 0 Nothing Nothing)

statusToBool :: String -> Bool
statusToBool "passed" = True
statusToBool _ = False