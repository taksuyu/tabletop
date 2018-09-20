{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Tabletop.Session where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text.Conversions as TC
import qualified Data.UUID as U
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS
import qualified Web.Cookie as C

import Control.Monad.Reader
import Data.Time.Clock
import Katip
import Lens.Micro.Platform
import Network.HTTP.Date
import Network.HTTP.Types.Header
import System.Random
import UnliftIO
import Web.PathPieces (toPathPiece)
import Web.ServerSession.Backend.Persistent
import Web.ServerSession.Core
import Web.ServerSession.Core.Internal (State(generator), generateSessionId)

data SessionConn = SessionConn
  { sessionId :: BS.ByteString
  , connection :: WS.Connection
  }

checkSession
  :: (KatipContext m, MonadReader e m, MonadUnliftIO m)
  => (e -> SqlStorage SessionMap) -> Wai.Request -> WS.PendingConnection -> m SessionConn
checkSession getStorage request pendingConnection = do
  env <- ask

  let sqlStorage = getStorage env
  state <- liftIO $ createState sqlStorage

  (sessionData, saveSessionToken) <- liftIO $
    loadSession @(SqlStorage SessionMap) state $ getCookieValue state request
  mSession <- liftIO $
    saveSession state saveSessionToken sessionData

  case mSession of
    Just session@Session{ sessionAuthId = Just authId } ->
      fmap (SessionConn authId) $
        liftIO . WS.acceptRequestWith pendingConnection $
          addSession state session

    _ -> do
      session@Session{ sessionAuthId = Just authId } <-
        createSqlSession state sessionData

      connection <- liftIO $ do
        runTransactionM sqlStorage $ insertSession sqlStorage session
        WS.acceptRequestWith pendingConnection $
          addSession state session

      pure (SessionConn authId connection)

createSqlSession
  :: MonadIO m
  => State (SqlStorage SessionMap) -> SessionMap
  -> m (Session SessionMap)
createSqlSession state sessionData = do
  uuid <- fmap U.toText (liftIO randomIO)

  -- NOTE: May not make much of a difference, but trying to force
  -- strictness of dSession so that the time we get is before
  -- evaluation of the time.
  let dSession = decomposeSession uuid sessionData
  time <- dSession `seq` liftIO getCurrentTime

  case dsAuthId $ dSession of
    Nothing -> do
      sessionId <- liftIO $ generateSessionId (generator state)
      pure $ Session
        sessionId
        (Just . TC.unUTF8 $ TC.convertText uuid)
        (dsDecomposed dSession)
        time
        time
    Just _ ->
      createSqlSession state sessionData

-- FIXME: We need tests that makes sure this session is properly made
addSession :: State sto -> Session sess -> WS.AcceptRequest
addSession state session = WS.defaultAcceptRequest & setAcceptHeaders %~ \ hs ->
  let sessionVal = getCookieName state
        `mappend` "="
        `mappend` (toPathPiece $ sessionKey session)
      cookie = TC.unUTF8 . TC.fromText $
        sessionVal
        `mappend` expires
        `mappend` httpOnly
      httpOnly = if getHttpOnlyCookies state
        then "; HttpOnly"
        else ""
      expires =
        maybe "" ("; Expires=" `mappend`)
          (TC.decodeText . TC.UTF8 . formatHTTPDate . utcToHTTPDate
            =<< cookieExpires state session)
  in (hSetCookie, cookie) : hs

setAcceptHeaders :: Lens WS.AcceptRequest WS.AcceptRequest WS.Headers WS.Headers
setAcceptHeaders fn request =
  fmap (\a -> request { WS.acceptHeaders = a }) (fn (WS.acceptHeaders request))

-- | Pull the value of our session cookie out of the `Wai.Request` by our
-- state's cookieName.
getCookieValue :: Storage sto => State sto -> Wai.Request -> Maybe BS.ByteString
getCookieValue state request = L.lookup cookieName . C.parseCookies
  =<< L.lookup hCookie (Wai.requestHeaders request)
  where
    cookieName = TC.unUTF8 . TC.fromText $ getCookieName state
