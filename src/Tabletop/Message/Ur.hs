{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tabletop.Message.Ur where

import GHC.Generics
  ( Generic )

import Data.Aeson
  ( FromJSON, ToJSON )
import Data.Text
  ( Text )
import Game.Ur

import Tabletop.Message

-- TODO: Consider making this a newtype around Turn
data Player
  = PlayerBlack
  | PlayerWhite
  deriving (Eq, Generic, Show)

instance FromJSON Player
instance ToJSON Player

turnToPlayer :: Turn -> Player
turnToPlayer = \case
  Black -> PlayerBlack
  White -> PlayerWhite

type UrTabletopMessage = TabletopMessage SystemMessage Text UrMessage

data UrMessage
  = Join Player
  | Move Int
  | PassTurn
  | GetCurrentGame
  deriving (Generic, Show)

instance FromJSON UrMessage

-- NOTE: Used for debugging
instance ToJSON UrMessage

data NextTurn
  = NextTurn Turn Int
  deriving (Generic, Show)

instance ToJSON NextTurn

type UrTabletopResponse = TabletopResponse ChannelInfo UrResponse

data UrResponse
  = JoinSuccess Player
  | JoinSuccessOther Player
  | JoinFailure Text
  | MoveSuccess Board
  | MoveFailure Text
  | PassSuccess NextTurn
  | PassFailure Text
  | CurrentGame Board
  | PlayerHasWon Player
  deriving (Generic, Show)

instance ToJSON UrResponse

deriving instance Generic Board
instance ToJSON Board

deriving instance Generic Side
instance ToJSON Side

deriving instance Generic Turn
instance ToJSON Turn
