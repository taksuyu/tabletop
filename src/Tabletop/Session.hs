module Tabletop.Session where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text.Conversions as TC
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS
import qualified Web.Cookie as C

import Control.Monad.Reader
import Data.Time.Clock
import Database.Redis
import Katip
import Lens.Micro.Platform
import Network.HTTP.Date
import Network.HTTP.Types.Header
import System.Exit
import UnliftIO
import Web.PathPieces (toPathPiece)
import Web.ServerSession.Backend.Redis
import Web.ServerSession.Core
import Web.ServerSession.Core.Internal (State(generator), generateSessionId)

import Tabletop.Common

data SessionConn = SessionConn
  { sessionId :: BS.ByteString
  , connection :: WS.Connection
  }

checkSession :: MonadUnliftIO m => Wai.Request -> WS.PendingConnection -> Tabletop m SessionConn
checkSession request pendingConnection = do
  env <- ask

  redisConnection <- initRedis

  -- TODO: We currently only use sessions for rejoining games, but we should
  -- consider timeouts in the future.
  let redisStorage = RedisStorage @SessionMap redisConnection Nothing Nothing
  state <- liftIO $ createState redisStorage

  (sessionData, saveSessionToken) <- liftIO $
    loadSession @(RedisStorage SessionMap) state $ getCookieValue state request
  mSession <- liftIO $
    saveSession state saveSessionToken sessionData

  case mSession of
    Just session@Session{ sessionAuthId = Just authId } -> do
      connection <- liftIO
        . WS.acceptRequestWith pendingConnection
        $ addSession state session
      pure (SessionConn authId connection)

    _ -> do
      now <- liftIO $ getCurrentTime

      -- We continue to increment the counter till we come across a session that
      -- isn't currently used. Normally reused session counter ids wouldn't be a
      -- problem, but redis is persistent while tabletop currently is not.
      let increment = atomically $ do
            n <- readTVar (env ^. sessionCounter)
            modifyTVar (env ^. sessionCounter) (+ 1)
            pure (TC.toText $ show n)

      sessionId <- liftIO $ generateSessionId (generator state)

      let checkAuthId sessCount =
            let dSession = decomposeSession sessCount sessionData
            in case dsAuthId $ dSession of
               Just _ -> pure $ Session @SessionMap
                 sessionId
                 (Just . TC.unUTF8 $ TC.convertText sessCount)
                 (dsDecomposed dSession)
                 now
                 now
               Nothing -> do
                 n <- increment
                 checkAuthId n

      session@Session{ sessionAuthId = Just authId } <- do
        n <- increment
        checkAuthId n

      connection <- liftIO $ do
        runTransactionM redisStorage $ insertSession redisStorage session
        WS.acceptRequestWith pendingConnection
          $ addSession state session

      pure (SessionConn authId connection)

-- | Connect to Redis so that we can store our sessions independent of the
-- application.
--
-- TODO: Take Redis connection info from a config file
initRedis :: MonadUnliftIO m => Tabletop m Connection
initRedis = withException (liftIO $ checkedConnect defaultConnectInfo) $ \e -> do
  $(logTM) ErrorS . ls $ displayException @SomeException e
  liftIO $ exitFailure

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
