module Verify.Assertion (
    Assertion,
    getBasecaseAssertion,
    getInductionAssertion
) where

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

getBasecaseAssertion :: BL.ByteString -> Assertion
getBasecaseAssertion = (mapBasecaseToAssertion . getBasecaseLogs) . (T.decodeUtf8 . B.concat . BL.toChunks)

getInductionAssertion :: BL.ByteString -> Assertion
getInductionAssertion = (mapInductionToAssertion . getInductionLogs) . (T.decodeUtf8 . B.concat . BL.toChunks)

mapBasecaseToAssertion :: [Basecase] -> Assertion
mapBasecaseToAssertion = (mapToAssertion nextState) . getValidBasecaseEvents
    where
        nextState :: Assertion -> Basecase -> Assertion
        nextState (Assertion _ step name trace) (BasecaseStatus passed) = 
            Assertion (statusToBool passed) step name trace
        
        nextState (Assertion passed _ name trace) (AssertionStep step) = 
            Assertion passed step name trace
        
        nextState (Assertion passed step _ trace) (AssertionFailed _ name) = 
            Assertion passed step (Just name) trace
        
        nextState (Assertion passed step name _) (AssertionWritingVCD trace) = 
            Assertion passed step name (Just trace)
        
        nextState _ _ = error "error creating assertion"

        getValidBasecaseEvents :: [Basecase] -> [Basecase]
        getValidBasecaseEvents = filter isBasecaseValid
            where
                isBasecaseValid :: Basecase -> Bool
                isBasecaseValid (BasecaseStatus _) = True
                isBasecaseValid (AssertionStep _) = True
                isBasecaseValid (AssertionFailed _ _) = True
                isBasecaseValid (AssertionWritingVCD _) = True
                isBasecaseValid _ = False

mapInductionToAssertion :: [Induction] -> Assertion
mapInductionToAssertion = (mapToAssertion nextState) . getValidInductionEvents
    where
        nextState :: Assertion -> Induction -> Assertion
        nextState (Assertion _ step name trace) (InductionStatus passed) = 
            Assertion (statusToBool passed) step name trace
        
        nextState (Assertion passed _ name trace) (InductionStep step) = 
            Assertion passed step name trace
        
        nextState _ _ = error "error creating assertion"

        getValidInductionEvents :: [Induction] -> [Induction]
        getValidInductionEvents = filter isInductionValid
            where
                isInductionValid :: Induction -> Bool
                isInductionValid (InductionStatus _) = True
                isInductionValid (InductionStep _) = True
                isInductionValid _ = False

mapToAssertion :: (Assertion -> a -> Assertion) -> [a] -> Assertion
mapToAssertion nextState = foldl nextState (Assertion False 0 Nothing Nothing)

statusToBool :: String -> Bool
statusToBool "passed" = True
statusToBool _ = False