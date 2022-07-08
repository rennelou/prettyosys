{-# LANGUAGE OverloadedStrings #-}

module Settings.Settings (
  Settings(..),
  getSettings,
  createSettings
) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Toml (TomlCodec, (.=))

import qualified Data.Text.IO as TIO
import qualified Toml

data Settings = Settings
    { settingsWorkDir   :: !Text
    , settingsSrcDir    :: !Text
    , settingsVunitsDir :: !Text
    , settingsDepht     :: !Int
    }

newtype WorkDir   = WorkDir Text
newtype SrcDir    = SrcDir Text
newtype VunitsDir = VunitsDir Text
newtype Depht     = Depht Int

getSettings :: Text -> Settings
getSettings s =
  case Toml.decode settingsCodec s of
    Left  err      -> error $ T.unpack (Toml.prettyTomlDecodeErrors err)
    Right settings -> settings

createSettings :: Settings -> Text
createSettings = Toml.encode settingsCodec

settingsCodec :: TomlCodec Settings
settingsCodec = Settings
    <$> Toml.text             "workdir"   .= settingsWorkDir
    <*> Toml.text             "srcdir"    .= settingsSrcDir
    <*> Toml.text             "vunitsdir" .= settingsVunitsDir
    <*> Toml.diwrap (Toml.int "depht")    .= settingsDepht