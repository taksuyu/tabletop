module Tabletop.Session where

import Data.ByteString
import Data.Text
import Data.Vault.Lazy as V
import Database.Redis
import Katip
import Network.Wai
import Network.Wai.Session
import System.Exit
import UnliftIO
import Web.ServerSession.Backend.Redis
import Web.ServerSession.Frontend.Wai

import Tabletop.Common

tabletopSessions :: MonadUnliftIO m => V.Key (Session m Text ByteString) -> Tabletop m Middleware
tabletopSessions k =
  withException
    (do
        a <- liftIO $ checkedConnect defaultConnectInfo
        liftIO $ withServerSession k id (RedisStorage a Nothing Nothing)
    )
    (\e -> do
        $(logTM) ErrorS . ls $ displayException @SomeException e
        liftIO $ exitFailure
    )
