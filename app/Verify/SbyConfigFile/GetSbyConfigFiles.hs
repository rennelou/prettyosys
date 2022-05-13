module Verify.SbyConfigFile.GetSbyConfigFiles (
    getSbyConfigFiles
) where

import Verify.SbyConfigFile.SbyConfigFile
import Parsers.PSL
import Parsers.FileExtensions.VHDL
import Parsers.FileExtensions.PSL
import Utils.FileSearch

getSbyConfigFiles :: SbyConfigArgs-> String -> String -> IO [SbyConfigFile]
getSbyConfigFiles args srcPath vunitPath = do
    (srcFiles, srcPaths) <- getVHDLSrcs srcPath
    vunits <- getVunits srcPath vunitPath
    return (
        map
            (\ (psl, file, path) -> 
                SbyConfigFile 
                    (getTopLevel psl)
                    (fileConcat srcFiles file)
                    (pathConcat srcPaths path)
                    args 
            )
            vunits 
        )


fileConcat :: String -> String -> String
fileConcat files file = files ++ " " ++ file

pathConcat :: String -> String -> String
pathConcat files file = files ++ "\n" ++ file