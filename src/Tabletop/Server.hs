{-# LANGUAGE TemplateHaskell #-}

module Tabletop.Server where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Pool
import Data.Proxy
import Database.Persist.Sqlite
import Database.Persist.TH
import Katip
import Network.Wai.Handler.Warp
import System.IO
import UnliftIO
import Web.ServerSession.Backend.Persistent
import Web.ServerSession.Core

import Tabletop.Common
import Tabletop.Config
import Tabletop.Socket

mkMigrate "migrateAll" (serverSessionDefs (Proxy :: Proxy SessionMap))

setupDatabase :: (MonadLogger m, MonadUnliftIO m) => m (SqlStorage SessionMap)
setupDatabase = do
  connectionPool <- createSqlitePool "sessions.sqlite3" 10 :: (MonadLogger m, MonadUnliftIO m) => m (Pool SqlBackend)
  runSqlPool (runMigration migrateAll) connectionPool
  pure (SqlStorage connectionPool)

startTabletop :: IO ()
startTabletop = do
  ttSessionStorage <- runNoLoggingT setupDatabase

  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "Tabletop" "product"
  bracket makeLogEnv closeScribes $ \le -> do
    let ns = "Server"
    (env, port) <- runKatipContextT le () ns $ do
      ttBackhand <- newTabletopBackhand
      ttConfig <- readTabletopConfig "tabletop-config.ini"
      pure $ (Env ttBackhand ttConfig ttSessionStorage, _cfPort ttConfig)
    runReaderT (unTabletop tabletop) (env ns mempty le)
      >>= runSettings (settings port)

settings :: Int -> Settings
settings port = setPort port $ defaultSettings
