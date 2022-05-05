module Verify (
    Mode(..),
    sbyConfig,
    sbyCommand,
    sbyMode
) where
    import Text.Printf

    data Mode = CoverProve | Cover | Prove deriving (Read, Show);

    sbyCommand :: Mode -> Bool -> String -> String -> String
    sbyCommand mode backupFlag = 
        printf "sby --yosys \"yosys -m ghdl\" %s %s --prefix %s/%s" (sbyMode mode) (sbyBackupFlag backupFlag)

    sbyMode :: Mode -> String
    sbyMode Cover = "-T cover"
    sbyMode Prove = "-T prove"
    sbyMode _ = ""

    sbyBackupFlag :: Bool -> String
    sbyBackupFlag False = ""
    sbyBackupFlag True = "-b"

    sbyConfig :: Int -> String -> String -> String -> String
    sbyConfig depht files topLevel paths = 
        (sbyTasksConfig depht) ++
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