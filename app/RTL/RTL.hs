module RTL.RTL (
    generateRTL
) where

import Utils.FileSearch
import Data.List
import System.Process.Typed
import Control.Concurrent.STM (atomically)

generateRTL :: String -> IO ()
generateRTL toplevel = do
    (_, srcs) <- getVHDLSrcs "src"
    putStrLn srcs
