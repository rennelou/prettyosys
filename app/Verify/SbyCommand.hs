module Verify.SbyCommand (
    SbyCommandArgs(..),
    Mode(..),
    sbyCommandWithConfigFile,
) where
    import Verify.SbyConfigFile.SbyConfigFile
    import Text.Printf

    data SbyCommandArgs = SbyCommandArgs {
        mode        :: Mode,
        hasBackup   :: Bool,
        workdir     :: String
    }

    data Mode = CoverProve | Cover | Prove deriving (Read, Show);

    sbyCommandWithConfigFile :: SbyCommandArgs -> SbyConfigFile -> String
    sbyCommandWithConfigFile commandArgs sbyConfigFile =
        printf "echo \"%s\" | %s" 
            (show sbyConfigFile)
            (sbyCommand commandArgs (topLevel sbyConfigFile) )

    sbyCommand :: SbyCommandArgs -> String -> String
    sbyCommand SbyCommandArgs{mode=mode, hasBackup=hasBackup, workdir=workdir} = 
        printf 
            "sby --yosys \"yosys -m ghdl\" %s %s --prefix %s/%s"
            (sbyMode mode)
            (sbyBackupFlag hasBackup)
            workdir

    sbyMode :: Mode -> String
    sbyMode Cover = "-T cover"
    sbyMode Prove = "-T prove"
    sbyMode _ = ""

    sbyBackupFlag :: Bool -> String
    sbyBackupFlag False = ""
    sbyBackupFlag True = "-b"