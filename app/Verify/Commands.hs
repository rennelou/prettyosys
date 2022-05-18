module Verify.Commands (
    SbyCommandArgs(..),
    Mode(..),
    symbiyosys,
    ghdlLint
) where
    import Verify.Sby
    import Data.List
    import Text.Printf

    data SbyCommandArgs = SbyCommandArgs {
        mode        :: Mode,
        hasBackup   :: Bool,
        workdir     :: String
    }

    data Mode = CoverProve | Cover | Prove deriving (Read, Show);

    symbiyosys :: SbyCommandArgs -> Sby -> String
    symbiyosys commandArgs sbyConfigFile =
        printf "echo \"%s\" | %s" 
            (show sbyConfigFile)
            (symbiyosys' commandArgs (topLevel sbyConfigFile) )

    symbiyosys' :: SbyCommandArgs -> String -> String
    symbiyosys' SbyCommandArgs{mode=mode, hasBackup=hasBackup, workdir=workdir} = 
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

    ghdlLint :: Sby -> String
    ghdlLint sby = 
        "ghdl -c --std=08 " ++ (unwords $ paths sby) ++ " -e " ++ (topLevel sby)
       