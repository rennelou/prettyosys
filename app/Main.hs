{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import Verify.Verify

main :: IO ()
main = do
    args <- getCliOptions
    verifyAll args