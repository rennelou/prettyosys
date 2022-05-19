module View.CoverTable (
    createCoverTable
) where

import Verify.CoverPoint
import View.Utils
import Text.Layout.Table
import Text.Layout.Table.Cell.Formatted

createCoverTable :: [CoverPoint] -> String
createCoverTable [] = "Error executing Cover Task"
createCoverTable covers = coverTable (map coverRow covers)

coverTable :: [[Formatted String]] -> String
coverTable rows  = 
    tableString [def , def, def, def]
                unicodeS
                (titlesH ["Property Cover", "Status", "Step", "Trace"])
                (map rowG rows)

coverRow :: CoverPoint -> [Formatted String]
coverRow cp = [prettyCoverName cp, boolToReached cp, prettyStep cp, prettyTrace cp]

prettyCoverName :: CoverPoint -> Formatted String
prettyCoverName cp = formatted "" (_CPname cp) ""

boolToReached :: CoverPoint -> Formatted String
boolToReached cp = 
    case (_CPreached cp) of
        True -> green "Passed"
        False -> red "Failed"

prettyStep :: CoverPoint -> Formatted String
prettyStep cp =
    case (_CPstep cp) of
        (Just step) -> formatted "" (show step) ""
        Nothing -> formatted "" "" ""

prettyTrace :: CoverPoint -> Formatted String
prettyTrace cp =
    case (_CPtrace cp) of
        (Just trace) -> formatted "" trace ""
        Nothing -> formatted "" "" ""