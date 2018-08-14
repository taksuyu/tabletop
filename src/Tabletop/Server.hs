module Tabletop.Server where

import Control.Exception
import Control.Monad.Reader
import Katip
import Network.Wai.Handler.Warp
import System.IO

import Tabletop.Common
import Tabletop.Config
import Tabletop.Socket

-- TODO: Everything should run within Katip for proper logging.
startTabletop :: IO ()
startTabletop = do
  ttBackhand <- newTabletopBackhand
  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "Tabletop" "product"
  bracket makeLogEnv closeScribes $ \le -> do
    let ns = "Server"
    ttConfig <- runKatipContextT le () ns $ readTabletopConfig "tabletop-config.ini"
    runReaderT (unTabletop tabletop) (Env ttBackhand ttConfig ns mempty le)
      >>= runSettings (setPort (_cfPort ttConfig) defaultSettings)

settings :: Int -> Settings
settings port = setPort port $ defaultSettings
