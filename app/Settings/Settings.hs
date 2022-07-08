{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings.Settings (
  Settings(..),
  getSettings,
  settingsCodec
) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Toml (TomlCodec, (.=))

import qualified Data.Text.IO as TIO
import qualified Toml

data Settings = Settings
    { settingsPort        :: !Port
    , settingsDescription :: !Text
    , settingsCodes       :: [Int]
    , settingsMail        :: !Mail
    , settingsUsers       :: ![User]
    }

data Mail = Mail
    { mailHost           :: !Host
    , mailSendIfInactive :: !Bool
    }

data User
    = Guest !Integer  -- id of guest
    | Registered !RegisteredUser  -- login and createdAt of registered user

data RegisteredUser = RegisteredUser
    { registeredUserLogin     :: !Text
    , registeredUserCreatedAt :: !Day
    }

newtype Port = Port Int
newtype Host = Host Text

getSettings :: Text -> Settings
getSettings s =
  case Toml.decode settingsCodec s of
    Left  err      -> error $ T.unpack (Toml.prettyTomlDecodeErrors err)
    Right settings -> settings

settingsCodec :: TomlCodec Settings
settingsCodec = Settings
    <$> Toml.diwrap (Toml.int  "server.port")       .= settingsPort
    <*> Toml.text              "server.description" .= settingsDescription
    <*> Toml.arrayOf Toml._Int "server.codes"       .= settingsCodes
    <*> Toml.table mailCodec   "mail"               .= settingsMail
    <*> Toml.list  userCodec   "user"               .= settingsUsers

mailCodec :: TomlCodec Mail
mailCodec = Mail
    <$> Toml.diwrap (Toml.text "host") .= mailHost
    <*> Toml.bool "send-if-inactive"   .= mailSendIfInactive

matchGuest :: User -> Maybe Integer
matchGuest = \case
   Guest i -> Just i
   _ -> Nothing

matchRegistered :: User -> Maybe RegisteredUser
matchRegistered = \case
   Registered u -> Just u
   _ -> Nothing

userCodec :: TomlCodec User
userCodec =
        Toml.dimatch matchGuest      Guest      (Toml.integer "guestId")
    <|> Toml.dimatch matchRegistered Registered registeredUserCodec

registeredUserCodec :: TomlCodec RegisteredUser
registeredUserCodec = RegisteredUser
    <$> Toml.text "login"     .= registeredUserLogin
    <*> Toml.day  "createdAt" .= registeredUserCreatedAt