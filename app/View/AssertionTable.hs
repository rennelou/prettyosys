module View.AssertionTable (
    createAssertionTable
) where

import Verify.Assertion
import View.Utils
import Text.Layout.Table
import Text.Layout.Table.Cell.Formatted

createAssertionTable :: Assertion -> Assertion -> String
createAssertionTable basecase induction =
    assertionTable [assertionRow basecase, assertionRow induction]

assertionTable :: [[Formatted String]] -> String
assertionTable rows  = 
    tableString [def , def, def, def]
                unicodeS
                (titlesH ["Status", "Step", "Assertion Failed", "Trace"])
                (map rowG rows)

assertionRow :: Assertion -> [Formatted String]
assertionRow ass = [boolToPass ass, prettyStep ass, prettyAssertionName ass, prettyTrace ass]

boolToPass :: Assertion -> Formatted String
boolToPass ass = 
    case (_ATpassed ass) of
        True -> green "Passed"
        False -> red "Failed"

prettyStep :: Assertion -> Formatted String
prettyStep ass = formatted "" (show (_ATstep ass)) ""

prettyAssertionName :: Assertion -> Formatted String
prettyAssertionName ass =
    case (_ATassertionFailed ass) of
        (Just s) -> formatted "" s ""
        Nothing -> formatted "" "" ""

prettyTrace :: Assertion -> Formatted String
prettyTrace ass =
    case (_ATtrace ass) of
        (Just trace) -> formatted "" trace ""
        Nothing -> formatted "" "" ""