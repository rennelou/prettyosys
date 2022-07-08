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
settingsTests = testGroup "Settings Tests" [getSettingsTest]

getSettingsTest :: TestTree
getSettingsTest =
  testCase  "Get Settings" ( do
      let settings = getSettings . T.pack
            $   "workdir   = \"verify_build\"\n"
            ++  "srcdir    = \"src\"\n"
            ++  "vunitsdir = \"verification_units\"\n"
            ++  "depht     = 50\n"
      TIO.putStrLn $ Toml.encode settingsCodec settings
    )