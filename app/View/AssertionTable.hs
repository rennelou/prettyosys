module View.AssertionTable (
    createAssertionTable
) where

import Verify.Assertion
import View.Utils
import Text.Layout.Table
import Text.Layout.Table.Cell.Formatted

createAssertionTable :: Assertion -> Assertion -> String
createAssertionTable (Assertion False 0 Nothing Nothing) (Assertion False 0 Nothing Nothing) = "Error Executing Prove Task"
createAssertionTable basecase induction =
    assertionTable [
        assertionRow "Basecase" basecase, 
        assertionRow "Induction" induction ]

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
    case (_ATpassed ass) of
        True -> green "Passed"
        False -> red "Failed"

prettyStep :: Assertion -> Formatted String
prettyStep ass = simpleFormettedString (show (_ATstep ass))

prettyAssertionName :: Assertion -> Formatted String
prettyAssertionName ass =
    case (_ATassertionFailed ass) of
        (Just s) -> simpleFormettedString s
        Nothing -> simpleFormettedString ""

prettyTrace :: Assertion -> Formatted String
prettyTrace ass =
    case (_ATtrace ass) of
        (Just trace) -> simpleFormettedString trace
        Nothing -> simpleFormettedString ""

simpleFormettedString :: String -> Formatted String
simpleFormettedString s = formatted "" s ""