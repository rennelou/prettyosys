module Verify.Commands (
    SbyCommandArgs(..),
    Mode(..),
    symbiyosys,
    yosysLint
) where
    import Verify.Sby
    import Data.List
    import Text.Printf

    data SbyCommandArgs = SbyCommandArgs {
        mode        :: Mode,
        hasBackup   :: Bool
    }

    data Mode = CoverProve | Cover | Prove deriving (Read, Show);

    symbiyosys :: SbyCommandArgs -> Sby -> String
    symbiyosys commandArgs sbyConfigFile =
        printf "echo \"%s\" | %s"
            (show sbyConfigFile)
            (symbiyosys' commandArgs (topLevel sbyConfigFile))

    symbiyosys' :: SbyCommandArgs -> String -> String
    symbiyosys' SbyCommandArgs{mode=mode, hasBackup=hasBackup} =
        printf
            "sby --yosys \"yosys -m ghdl\" %s %s --prefix %s"
            (sbyMode mode)
            (sbyBackupFlag hasBackup)

    sbyMode :: Mode -> String
    sbyMode Cover = "-T cover"
    sbyMode Prove = "-T prove"
    sbyMode _ = ""

    sbyBackupFlag :: Bool -> String
    sbyBackupFlag False = ""
    sbyBackupFlag True = "-b"

    yosysLint :: Sby -> String
    yosysLint sby =
        "yosys -m ghdl -qp " ++
        "'" ++
        "ghdl --std=08 " ++ unwords (paths sby) ++
        " -e "++ topLevel sby ++"; " ++
        "prep -top " ++ topLevel sby ++ "; " ++
        "hierarchy -simcheck" ++
        "'"