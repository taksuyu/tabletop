{-# LANGUAGE TemplateHaskell #-}

module Tabletop.Server where

import qualified Data.Text as T

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Proxy
import Database.Persist.Sqlite
import Database.Persist.TH
import Katip
import Katip.Instances.MonadLogger ()
import Network.Wai.Handler.Warp
import System.IO
import UnliftIO
import Web.ServerSession.Backend.Persistent
import Web.ServerSession.Core

import Tabletop.Common
import Tabletop.Config
import Tabletop.Parser
import Tabletop.Socket

mkMigrate "migrateAll" (serverSessionDefs (Proxy @SessionMap))

setupDatabase :: (MonadLogger m, MonadUnliftIO m) => T.Text -> m (SqlStorage SessionMap)
setupDatabase sqlitePath' = do
  connectionPool <- createSqlitePool sqlitePath' 10
  runSqlPool (runMigration migrateAll) connectionPool
  pure (SqlStorage connectionPool)

startTabletop :: IO ()
startTabletop = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "Tabletop" "product"
  bracket makeLogEnv closeScribes $ \le -> do
    let ns = "Server"
    (env, port) <- runKatipContextT le () ns $ do
      ttBackhand <- newTabletopBackhand
      ttOptions <- parseOptions
      ttConfig <- readConfig (_configPath ttOptions)
      ttSessionStorage <- setupDatabase (maybe (_cfSqlitePath ttConfig) id (_sqlitePath ttOptions))
      pure $ (Env ttBackhand ttConfig ttSessionStorage, _cfGeneralPort ttConfig)
    runReaderT (unTabletop tabletop) (env ns mempty le)
      >>= runSettings (settings port)

settings :: Int -> Settings
settings port =
  setPort port $ defaultSettings
