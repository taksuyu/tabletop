{-# LANGUAGE TemplateHaskell #-}

module Tabletop.Config where

import qualified Data.Text.IO as TIO
import qualified Lens.Micro.Platform as LMP
import qualified System.Directory as Dir

import Data.Typeable
import System.Exit

import Data.Ini.Config.Bidir
import Katip
import System.FilePath
import UnliftIO

data TabletopSession
  = Memory
    -- ^ Use a SessionMap from server-session. These don't persist
    -- through reloads and make a good default when tabletop is being
    -- ran together as a single process.
  | Redis
    -- ^ Use RedisStorage to store our server sessions. This works
    -- well when you want to persist sessions over a longer period of
    -- time such as when we have game sessions running on separate
    -- processes and the socket server goes through a restart.
  deriving (Eq, Show, Read, Typeable)

data TabletopConfig = TabletopConfig
  { -- General
    _cfPort :: Int
  , _cfSession :: TabletopSession

    -- Redis
  , _cfRedisPort :: Int
  } deriving (Eq, Show)

LMP.makeLenses ''TabletopConfig

configParser :: IniSpec TabletopConfig ()
configParser = do
  allOptional (section "general") $ do
    cfPort .= field "port" number & placeholderValue "3000"
    cfSession .= field "session" readable & placeholderValue "Memory"
  allOptional (section "redis") $ do
    cfRedisPort .= field "port" number & placeholderValue "6379"

defaultConfig :: Ini TabletopConfig
defaultConfig = ini defaults configParser
  where
    defaults = TabletopConfig
      { _cfPort = 3000
      , _cfSession = Memory
      , _cfRedisPort = 6379
      }

readTabletopConfig :: (MonadUnliftIO m, KatipContext m) => FilePath -> m TabletopConfig
readTabletopConfig p =
  readIni `catchIO` \_ -> do
    cwd <- liftIO Dir.getCurrentDirectory
    $(logTM) InfoS . logStr $ "Config file wasn't found. Creating one at " ++ cwd </> p
    liftIO $ TIO.writeFile p (serializeIni defaultConfig)
    getPureIniValue defaultConfig
  where
    getPureIniValue = pure . getIniValue

    readIni = do
      file <- liftIO $ TIO.readFile p
      case parseIni file defaultConfig of
        Right config ->
          getPureIniValue config
        Left err -> do
          $(logTM) ErrorS $ logStr err
          $(logTM) InfoS "Tabletop is now exiting."
          liftIO exitFailure
