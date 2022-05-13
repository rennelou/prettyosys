module Verify.CoverPoint (
    CoverPoint,
    getCoverPoints
) where

import Parsers.SbyLog.SbyLog
import Text.Megaparsec hiding (State)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data CoverPoint = CoverPoint {
    _CPname     :: String,
    _CPreached  :: Bool,
    _CPstep     :: Integer,
    _CPtrace    :: String
} deriving (Show)

data CoverGroupState = CoverGroupState {
    buffer      :: [Cover],
    coverPoints :: [CoverPoint]
}

getCoverPoints :: BL.ByteString -> [CoverPoint]
getCoverPoints = (mapToCoverPoints . getCoverLogs) . (T.decodeUtf8 . B.concat . BL.toChunks)

mapToCoverPoints :: [Cover] -> [CoverPoint]
mapToCoverPoints  = coverPoints . (foldl nextState (CoverGroupState [] [])) . getValidCoverEvents
    where
        nextState :: CoverGroupState -> Cover -> CoverGroupState
        nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints}  reached@(ReachedCoverPoint _ _) =
            CoverGroupState (buffer ++ [reached]) coverPoints
        
        nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints} unreached@(UnreachdCoverPoint _) =
            CoverGroupState buffer (coverPoints ++ [createCoverPoint "" unreached])
        
        nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints} (WritingCoverVCD trace) =
            CoverGroupState [] (coverPoints ++ (map (createCoverPoint trace) buffer))
        
        createCoverPoint :: String -> Cover -> CoverPoint
        createCoverPoint trace (ReachedCoverPoint name step) = CoverPoint name True step trace
        createCoverPoint _ (UnreachdCoverPoint name) = CoverPoint name False (-1) ""
        createCoverPoint _ _ = error "error creating coverPoint"

getValidCoverEvents :: [Cover] -> [Cover]
getValidCoverEvents = filter isCoverValid
    where
        isCoverValid :: Cover -> Bool
        isCoverValid (ReachedCoverPoint _ _) = True
        isCoverValid (UnreachdCoverPoint _) = True
        isCoverValid (WritingCoverVCD _) = True
        isCoverValid _ = False