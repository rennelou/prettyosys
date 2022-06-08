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
    symbiyosys args sbyConfigFile =
        printf "echo \"%s\" | %s"
            (show sbyConfigFile)
            (symbiyosys' args (topLevel sbyConfigFile))

    symbiyosys' :: SbyCommandArgs -> String -> String
    symbiyosys' args =
        printf
            "sby --yosys \"yosys -m ghdl\" %s %s --prefix %s"
            (sbyMode . mode $ args)
            (sbyBackupFlag . hasBackup $ args)

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