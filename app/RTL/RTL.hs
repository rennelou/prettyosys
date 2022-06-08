module RTL.RTL (
    RTLArgs(..),
    generateRTL
) where

import Utils.FileExtensionSearch
import Data.List
import Text.Printf
import System.Process.Typed
import Control.Concurrent.STM (atomically)

data RTLArgs = RTLArgs {
    toplevel    :: String,
    workdir     :: String
}

generateRTL :: RTLArgs -> IO ()
generateRTL RTLArgs { toplevel=toplevel, workdir=workdir } = do
    paths <- unwords <$> getFiles ["vhd", "vhdl"] "src"
    runProcess_ $ shell $ yosysRTL paths toplevel workdir

yosysRTL :: String -> String -> String -> String
yosysRTL srcs toplevel workdir = 
    "cd " ++ workdir ++ "; " ++
    "yosys -m ghdl -p 'ghdl " ++
    srcs ++
    " -e "++
    toplevel ++
    "; prep; show -colors 42 -stretch -format svg -prefix " ++
    toplevel ++
    " show " ++
    toplevel ++
    "'"
