module Tabletop.Server where

import Control.Exception
import Control.Monad.Reader
import Katip as K
import Network.Wai.Handler.Warp
import System.IO

import Tabletop.Common
import Tabletop.Socket

startTabletop :: IO ()
startTabletop = do
  ttBackhand <- newTabletopBackhand
  handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "Tabletop" "product"
  bracket makeLogEnv closeScribes $ \le -> do
    let initialNamespace = "main"
    runReaderT (unTabletop tabletop) (Env ttBackhand initialNamespace mempty le)
      >>= runSettings settings

settings :: Settings
settings =
  let port = 3000 in
  setPort port
  . setBeforeMainLoop
    ( hPutStrLn stderr
      ( "listening on port " ++ show port ++ "..." ))
  $ defaultSettings
