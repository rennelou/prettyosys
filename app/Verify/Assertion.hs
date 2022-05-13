module Verify.Assertion (
    Assertion,
    getAssertion
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

getAssertion :: BL.ByteString -> Assertion
getAssertion = (mapToAssertion . getBasecaseLogs) . (T.decodeUtf8 . B.concat . BL.toChunks)

mapToAssertion :: [Basecase] -> Assertion
mapToAssertion = (foldl nextState (Assertion False 0 Nothing Nothing)) . getValidBasecaseEvents
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

        statusToBool :: String -> Bool
        statusToBool "passed" = True
        statusToBool _ =False