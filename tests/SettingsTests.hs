{-# LANGUAGE OverloadedStrings #-}

module SettingsTests (
  settingsTests
) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import qualified Toml
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Settings.Settings

settingsTests :: TestTree
settingsTests = testGroup "Settings Tests" [getSettingsTest, createSettingsTest]

getSettingsTest :: TestTree
getSettingsTest =
  testCase  "Get Settings" ( do
      let settings = getSettings . T.pack
            $   "workdir   = \"verify_build\"\n"
            ++  "srcdir    = \"src\"\n"
            ++  "vunitsdir = \"verification_units\"\n"
            ++  "depht     = 50\n"
      return ()
    )

createSettingsTest :: TestTree
createSettingsTest = 
  testCase "Create Settings" ( do
      let settings = Settings "Verify_build" "src" "verification_units" 50
      let toml = createSettings settings
      return ()
    )