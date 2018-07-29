module Tabletop.Config where

import Data.Typeable

import Data.Ini.Config.Bidir
import qualified Lens.Micro.Platform as LMP

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
  section "General" $ do
    cfPort .= field "port" number
    cfSession .= field "session" readable
  section "Redis" $ do
    cfRedisPort .=  field "port" number

defaultConfig :: Ini TabletopConfig
defaultConfig = ini defaults configParser
  where
    defaults = TabletopConfig
      { _cfPort = 3000
      , _cfSession = Memory
      , _cfRedisPort = 6379
      }
