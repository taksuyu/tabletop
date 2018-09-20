{-# LANGUAGE DuplicateRecordFields, FlexibleContexts #-}

module Tabletop.Socket where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Katip
import Lens.Micro.Platform
import Network.HTTP.Types
import Network.Wai as Wai
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import UnliftIO
import UnliftIO.Concurrent

import Backhand as B
import Backhand.Modules.Ur

import Tabletop.Common
import Tabletop.Message
import Tabletop.Message.Ur
import Tabletop.Session

tabletop :: Tabletop IO Application
tabletop = do
  env <- ask
  return $ \request respond ->
    let app pendingConnection =
          case requestPath $ pendingRequest pendingConnection of
            "/games/ur" -> flip runReaderT env . unTabletop $ do
              $(logTM) DebugS "Detected Ur WebSocket connection"
              sessionConn <- checkSession _sessionStorage request pendingConnection
              urGameHandler sessionConn
            _ -> rejectRequest pendingConnection "Not a valid path."

    in case websocketsApp defaultConnectionOptions app request of
      Nothing  -> respond (responseLBS status400 [] "Not a WebSocket request")
      Just res -> respond res

urGameHandler :: SessionConn -> Tabletop IO ()
urGameHandler SessionConn{ sessionId, connection } = do
  env <- ask

  client <- liftIO $ newDefaultClient @UrTabletopResponse
  channels <- newTVarIO []
  thread <- forkFinally
    (messageSender client connection)
    $ \ _ -> liftIO $ do
      chans <- atomically $ readTVar channels
      mapM_ (leaveChannel (env ^. bhand . urChannels) client) chans

  readFromConnection thread $ loopRetrieveData channels client thread
  where
    -- FIXME: We need to separate this out from one specific handler and
    -- generalize it for our games.
    loopRetrieveData :: TVar [UUID] -> Client UrTabletopResponse -> ThreadId -> Tabletop IO ()
    loopRetrieveData channels client@Client{ unique } thread = do
      env <- ask
      let channelMap = env ^. bhand . urChannels

      bstring <- liftIO $ receiveData connection
      case decode' @UrTabletopMessage bstring of
        Just msg ->
          case msg of
            SystemMessage message -> do
              $(logTM) DebugS "Got SystemMessage"
              case message of
                CreateGame ->
                  urGameChannel
                    >>= liftIO . sendTextData connection . encode @UrTabletopResponse . SystemResponse
            ServiceMessage uuid service message -> do
              $(logTM) DebugS "Got ServiceMessage"
              channelExists <- liftIO $ isChannelPresent channelMap uuid
              channelAlreadyJoined <- atomically $ do
                l <- readTVar channels
                if uuid `elem` l
                  then pure True
                  else pure False
              if channelExists && not channelAlreadyJoined
                then liftIO $ void . atomically $ do
                  modifyTVar channels (uuid :)
                  joinChannel client uuid channelMap
                else pure ()
              liftIO $ sendMessage channelMap (ConnectionData uuid service (TMessager unique sessionId message))
                >>= \case
                  Left err ->
                    -- FIXME: We could send a much better error message
                    sendTextData connection (encode err)
                  _ ->
                    pure ()
        _ ->
          $(logTM) DebugS "Didn't recieve a proper message"

      -- If we encountered an error then readFromConnection would catch us, so we'll
      -- continue on regardless of what happens.
      loopRetrieveData channels client thread

-- | We want to respond to the client as we get messages from the backhand
-- services. Currently this just sends back whatever we get from the service.
messageSender :: ToJSON a => Client a -> Connection -> Tabletop IO ()
messageSender client conn = forever $ do
  liftIO $ sendTextData conn . encode =<< readWith client
  $(logTM) DebugS "Sent message back over websocket"

-- | As we work with our WebSocket connection we may error out due to the client
-- closing the connection and we want to properly clean up when it does. If we
-- get a separate error we'll report it and continue the WebSocket loop.
readFromConnection :: (MonadUnliftIO m, KatipContext m) => ThreadId -> m () -> m ()
readFromConnection t fn = fn `catch` \case
  CloseRequest _ _ -> killThread t
  ConnectionClosed -> killThread t
  err -> do
    $(logTM) ErrorS (showLS err)
    fn
