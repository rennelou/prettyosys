module Verify.CoverPoint (
    CoverPoint(..),
    getCoverPoints
) where

import System.Directory

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
    _CPstep     :: Maybe Integer,
    _CPtrace    :: Maybe String
} deriving (Show)

data CoverGroupState = CoverGroupState {
    buffer      :: [Cover],
    coverPoints :: [CoverPoint]
}

getCoverPoints :: BL.ByteString -> IO [CoverPoint]
getCoverPoints text = do
    currentDirectory <- getCurrentDirectory
    return ((mapToCoverPoints . getCoverLogs currentDirectory . T.decodeUtf8 . B.concat . BL.toChunks) text)

mapToCoverPoints :: [Cover] -> [CoverPoint]
mapToCoverPoints  = coverPoints . foldl nextState (CoverGroupState [] []) . getValidCoverEvents
    where
        nextState :: CoverGroupState -> Cover -> CoverGroupState
        nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints}  reached@(ReachedCoverPoint _ _) =
            CoverGroupState (buffer ++ [reached]) coverPoints

        nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints} unreached@(UnreachdCoverPoint _) =
            CoverGroupState buffer (coverPoints ++ [createUnreachedCoverPoint unreached])

        nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints} (WritingCoverVCD trace) =
            CoverGroupState [] (coverPoints ++ map (createReachedCoverPoint trace) buffer)
        
        nextState _ _ = error "Invalid state when creating cover points"

        createReachedCoverPoint :: String -> Cover -> CoverPoint
        createReachedCoverPoint trace (ReachedCoverPoint name step) = CoverPoint name True (Just step) (Just trace)
        createReachedCoverPoint _ _ = error "error creating reached cover point"

        createUnreachedCoverPoint :: Cover -> CoverPoint
        createUnreachedCoverPoint (UnreachdCoverPoint name) = CoverPoint name False Nothing Nothing
        createUnreachedCoverPoint _ = error "error creating unreached coverPoint"

        getValidCoverEvents :: [Cover] -> [Cover]
        getValidCoverEvents = filter isCoverValid
            where
                isCoverValid :: Cover -> Bool
                isCoverValid (ReachedCoverPoint _ _) = True
                isCoverValid (UnreachdCoverPoint _) = True
                isCoverValid (WritingCoverVCD _) = True
                isCoverValid _ = False