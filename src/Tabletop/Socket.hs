{-# LANGUAGE DuplicateRecordFields, FlexibleContexts, TemplateHaskell #-}

module Tabletop.Socket where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Katip
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.STM

import Backhand as B
import Backhand.Modules.Ur

import Tabletop.Common
import Tabletop.Message
import Tabletop.Message.Ur

newTabletopBackhand :: IO TabletopBackhand
newTabletopBackhand =
  pure TabletopBackhand <*> newChannelsIO

tabletop :: Tabletop IO Application
tabletop = do
  $(logTM) NoticeS "Tabletop is starting!"
  env <- ask
  return $ websocketsOr defaultConnectionOptions (tabletopWS env) backupApp
    where
      tabletopWS env pendingConn =
        case requestPath $ pendingRequest pendingConn of
          "/games/ur" ->
            -- Maybe there is an easier way to embed another reader.
            runReaderT (unTabletop $ urGameHandler pendingConn) env
          -- "/games/chess" -> chessGameHandler (chessChannelMap bhand) pendingConn
          _ ->
            rejectRequest pendingConn "Not a valid path."

      backupApp :: Application
      backupApp _ respond =
        respond $ responseLBS status400 [] "Not a WebSocket request"

urGameHandler :: PendingConnection -> Tabletop IO ()
urGameHandler pconn = do
  conn <- liftIO $ acceptRequest pconn
  Env { bhand = TabletopBackhand { urChannelMap } } <- ask
  client <- liftIO $ newDefaultClient @UrTabletopResponse
  channels <- newTVarIO []
  thread <- forkFinally
    (messageSender client conn)
    $ \ _ -> liftIO $ do
      chans <- atomically $ readTVar channels
      mapM_ (leaveChannel urChannelMap client) chans
  liftIO $ forkPingThread conn 30
  readConnection thread $ loopRetrieveData channels client thread conn
  where
    readConnection t fn = fn `catch` \case
      CloseRequest _ _ -> killThread t
      ConnectionClosed -> killThread t
      _ -> fn

    loopRetrieveData :: TVar [UUID] -> Client UrTabletopResponse -> ThreadId -> Connection -> Tabletop IO ()
    loopRetrieveData channels client@Client{ unique } thread conn = do
      Env { bhand = TabletopBackhand { urChannelMap } } <- ask
      bstring <- liftIO $ receiveData conn
      case decode' @UrTabletopMessage bstring of
        Just msg ->
          case msg of
            SystemMessage message -> do
              $(logTM) InfoS "Got SystemMessage"
              case message of
                CreateGame ->
                  urGameChannel
                    >>= liftIO . sendTextData conn . encode @UrTabletopResponse . SystemResponse
            ServiceMessage uuid service message -> do
              $(logTM) InfoS "Got ServiceMessage"
              channelExists <- liftIO $ isChannelPresent urChannelMap uuid
              channelAlreadyJoined <- atomically $ do
                l <- readTVar channels
                if uuid `elem` l
                  then pure True
                  else pure False
              if channelExists && not channelAlreadyJoined
                then liftIO $ void . atomically $ do
                  modifyTVar channels (uuid :)
                  joinChannel client uuid urChannelMap
                else pure ()
              liftIO $ sendMessage urChannelMap (ConnectionData uuid service (Message unique message))
                >>= \case
                  Left err ->
                    -- FIXME: We could send a much better error message
                    sendTextData conn (encode err)
                  _ ->
                    pure ()
        _ ->
          $(logTM) InfoS "Didn't recieve a proper message"

      -- If we encountered an error then readConnection would catch us, so we'll
      -- continue on regardless of what happens.
      loopRetrieveData channels client thread conn

    messageSender :: ToJSON a => Client a -> Connection -> Tabletop IO ()
    messageSender client conn = forever $ do
      liftIO $ sendTextData conn . encode =<< readWith client
      $(logTM) InfoS "Sent message back over websocket"

chessGameHandler :: PendingConnection -> IO ()
chessGameHandler _ = pure ()
