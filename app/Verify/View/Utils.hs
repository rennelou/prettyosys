module Verify.View.Utils (
    green,
    red
) where

import Text.Layout.Table.Cell.Formatted

green :: String -> Formatted String
green s = formatted "\ESC[32m" s "\ESC[0m"

red :: String -> Formatted String
red s = formatted "\ESC[31m" s "\ESC[0m"