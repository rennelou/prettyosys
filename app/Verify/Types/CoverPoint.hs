module Verify.Types.CoverPoint (
    CoverPoint(..),
    getCoverPoints
) where

import Text.Megaparsec hiding (State)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe

import System.Directory
import Utils.Parsers.SbyLog
import Verify.Types.Utils

data CoverPoint = CoverPoint {
    _CPname     :: String,
    _CPreached  :: Bool,
    _CPstep     :: Maybe Integer,
    _CPtrace    :: Maybe String
} deriving (Show)

data CoverGroupState = CoverGroupState {
    buffer      :: [CoverLog],
    coverPoints :: [CoverPoint]
}

getCoverPoints :: FilePath -> T.Text -> [CoverPoint]
getCoverPoints workdir logs = coverPoints $ foldl nextState (CoverGroupState [] []) (getCoverLogs workdir logs)

nextState :: CoverGroupState -> CoverLog -> CoverGroupState
nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints}  reached@(CoverpointReached _ _) =
  CoverGroupState (buffer ++ [reached]) coverPoints
nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints} unreached@(CoverpointUnreachd _) =
  CoverGroupState buffer (coverPoints ++ [createUnreachedCoverPoint unreached])
nextState CoverGroupState {buffer=buffer,coverPoints=coverPoints} (CoverpointVCD trace) =
  CoverGroupState [] (coverPoints ++ map (createReachedCoverPoint trace) buffer)
nextState coverGroup _ = coverGroup

createReachedCoverPoint :: String -> CoverLog -> CoverPoint
createReachedCoverPoint trace (CoverpointReached name step) = CoverPoint name True (Just step) (Just trace)
createReachedCoverPoint _ _ = error "error creating reached cover point"

createUnreachedCoverPoint :: CoverLog -> CoverPoint
createUnreachedCoverPoint (CoverpointUnreachd name) = CoverPoint name False Nothing Nothing
createUnreachedCoverPoint _ = error "error creating unreached coverPoint"