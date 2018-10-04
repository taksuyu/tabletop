{-# LANGUAGE TemplateHaskell #-}

module Tabletop.Config where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Lens.Micro.Platform as LMP
import qualified System.Directory as Dir

import Control.Applicative
import Data.Typeable
import System.Exit

import Data.Ini.Config.Bidir
import Katip
import UnliftIO

-- | Sum type to determine how we are storing sessions. For now we
-- only support Sqlite, but in the future PostgreSQL will also be an
-- option.
data SessionStorage
  = Sqlite
    -- ^ Sqlite is the default storage system since it doesn't require setup.
  deriving (Eq, Show, Read, Typeable)

data Config = Config
  { -- General
    _cfGeneralPort :: Int
  , _cfGeneralSessionStorage :: SessionStorage

    -- Sqlite
  , _cfSqlitePath :: T.Text
  } deriving (Eq, Show)

LMP.makeLenses ''Config

configParser :: IniSpec Config ()
configParser = do
  section "General" $ do
    cfGeneralPort .= field "Port" number
    cfGeneralSessionStorage .= field "SessionStorage" readable

  allOptional (section "Sqlite") $ do
    cfSqlitePath .= field "Path" text

defaults :: Config
defaults =
  Config
  { _cfGeneralPort = 3000
  , _cfGeneralSessionStorage = Sqlite
  , _cfSqlitePath = "./sessions.tabletop.sqlite3"
  }

defaultIniConfig :: Ini Config
defaultIniConfig =
  ini defaults configParser

-- TODO: We want to read /etc/tabletop/config.ini and if that fails to
-- read from the current working directory. We'll then announce which
-- config we detected. If no config was detected we will use the defaults.
readConfig :: (MonadUnliftIO m, KatipContext m) => Maybe FilePath -> m Config
readConfig configPath = do
  mpath <- liftIO (pure configPath <|> Dir.findFile ["/etc/tabletop/", "./"] "config.ini")
  case mpath of
    Just path -> do
      $(logTM) InfoS . logStr $ "Detected config file: " ++ path
      file <- liftIO $ TIO.readFile path
      case parseIni file defaultIniConfig of
        Right config ->
          pure . getIniValue $ config
        Left err -> do
          $(logTM) ErrorS $ logStr err
          $(logTM) InfoS "Tabletop is now exiting."
          liftIO exitFailure
    Nothing -> do
      $(logTM) InfoS "No config file was detected. Default options are going to be used."
      pure . getIniValue $ defaultIniConfig
