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
            $   "server.port        = 8080\n"
            ++  "server.codes       = [ 5, 10, 42 ]\n"
            ++  "server.description = \"\"\"\n"
            ++  "This is production server.\n"
            ++  "Don't touch it!\n"
            ++  "\"\"\"\n"
            ++  "\n"
            ++  "[mail]\n"
            ++  "host = \"smtp.gmail.com\"\n"
            ++  "send-if-inactive = false\n"
            ++  "\n"
            ++  "[[user]]\n"
            ++  "guestId = 42\n"
            ++  "\n"
            ++  "[[user]]\n"
            ++  "guestId = 114\n"
            ++  "\n"
            ++  "[[user]]\n"
            ++  "login = \"Foo Bar\"\n"
            ++  "createdAt = 2020-05-19\n"
      TIO.putStrLn $ Toml.encode settingsCodec settings
    )