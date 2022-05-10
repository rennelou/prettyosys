module Verify.SbyConfigFile.SbyConfigFile (
    SbyConfigFile(..),
    SbyConfigArgs(..)
) where
import Text.Printf

data SbyConfigFile = SbyConfigFile {   
    topLevel    :: String,
    files       :: String,
    paths       :: String,
    configArgs  :: SbyConfigArgs
}

data SbyConfigArgs = SbyConfigArgs {
    depht       :: Int
}

instance Show SbyConfigFile where
    show SbyConfigFile {topLevel=topLevel, files=files, paths=paths, configArgs=args} = 
        (sbyTasksConfig (depht args)) ++
        sbyEngineConfig ++
        (sbyScriptsConfig files topLevel) ++
        (sbyFilesConfig paths)

sbyTasksConfig :: Int -> String
sbyTasksConfig depth =
    "[tasks]\n" ++
    "cover\n" ++
    "prove\n" ++
    "[options]\n" ++
    "cover: mode cover\n" ++
    (printf "cover: depth %d\n" depth) ++
    "prove: mode prove\n"

sbyEngineConfig :: String
sbyEngineConfig =
    "[engines]\n" ++
    "smtbmc\n"

sbyScriptsConfig :: String -> String -> String
sbyScriptsConfig paths topLevel =
    "[script]\n" ++
    (printf "ghdl --std=08 %s -e %s\n" paths topLevel) ++
    (printf "prep -top %s\n" topLevel)

sbyFilesConfig :: String -> String
sbyFilesConfig = printf "[files]\n%s"