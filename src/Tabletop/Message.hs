module Tabletop.Message where

import Backhand
import Data.Aeson
import Data.Aeson.Types
import Data.Text as T
import GHC.Generics

data TabletopMessage a s b
  = SystemMessage a
  | ServiceMessage UUID s b
  deriving (Generic, Show)

instance (FromJSON a, FromJSON s, FromJSON b) => FromJSON (TabletopMessage a s b)

-- NOTE: Used for debugging
instance (ToJSON a, ToJSON s, ToJSON b) => ToJSON (TabletopMessage a s b)

data SystemMessage
  = CreateGame
  -- JoinCurrentGame
  deriving (Generic, Show)

instance FromJSON SystemMessage where
  parseJSON (String t) = case T.unpack t of
    "CreateGame" -> pure CreateGame
    str -> fail ("Expected SystemMessage: got " `mappend` str)

  parseJSON invalid = typeMismatch "SystemMessage" invalid

data TabletopResponse a b
  = SystemResponse a
  | ServiceResponse b
  deriving (Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (TabletopResponse a b) where
  toJSON = \case
    SystemResponse a ->
      object [ "tag" .= T.pack "SystemResponse", "contents" .= a ]
    ServiceResponse a ->
      object [ "tag" .= T.pack "ServiceResponse", "contents" .= a ]

data ChannelInfo = ChannelInfo
  { channelUUID :: UUID
  , channelModules :: [Text]
  } deriving (Eq, Generic)

instance ToJSON ChannelInfo
