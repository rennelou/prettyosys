module View.AssertionTable (
    createAssertionTable
) where

import Verify.Types.Assertion
import View.Utils
import Text.Layout.Table
import Text.Layout.Table.Cell.Formatted

createAssertionTable :: [(String, Assertion)] -> String
createAssertionTable [] = "Error Executing Prove Task"
createAssertionTable assertions =
    assertionTable (map (uncurry assertionRow) assertions)

assertionTable :: [[Formatted String]] -> String
assertionTable rows  =
    tableString [def, def, def, def, def]
                unicodeS
                (titlesH ["Mode", "Status", "Step", "Assertion Failed", "Trace"])
                (map rowG rows)

assertionRow :: String -> Assertion -> [Formatted String]
assertionRow mode ass =
    [simpleFormettedString mode, boolToPass ass, prettyStep ass, prettyAssertionName ass, prettyTrace ass]

boolToPass :: Assertion -> Formatted String
boolToPass ass =
    if _ATpassed ass then green "Passed" else red "Failed"

prettyStep :: Assertion -> Formatted String
prettyStep ass = simpleFormettedString (show (_ATstep ass))

prettyAssertionName :: Assertion -> Formatted String
prettyAssertionName ass =
    case _ATassertionFailed ass of
        (Just s) -> simpleFormettedString s
        Nothing -> simpleFormettedString ""

prettyTrace :: Assertion -> Formatted String
prettyTrace ass =
    case _ATtrace ass of
        (Just trace) -> simpleFormettedString trace
        Nothing -> simpleFormettedString ""

simpleFormettedString :: String -> Formatted String
simpleFormettedString s = formatted "" s ""