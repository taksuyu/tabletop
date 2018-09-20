{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving,
  TypeFamilies, UndecidableInstances #-}

module Tabletop.Common where

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Monad.Reader
import Katip
import Lens.Micro.Platform
import UnliftIO
import Web.ServerSession.Backend.Persistent
import Web.ServerSession.Core

import Backhand

import Tabletop.Config
import Tabletop.Message.Ur

data TMessager a = TMessager
  { unique :: Unique
  , sessionId :: BS.ByteString
  , message :: a
  }

data TabletopBackhand = TabletopBackhand
  { _urChannels :: Channels T.Text (TMessager UrMessage) UrTabletopResponse
  }

makeLenses ''TabletopBackhand

newTabletopBackhand :: MonadIO m => m TabletopBackhand
newTabletopBackhand =
  atomically $ fmap TabletopBackhand newChannels

data Env = Env
  { _bhand :: TabletopBackhand
  , _config :: TabletopConfig
  , _sessionStorage :: SqlStorage SessionMap
  , _logNamespace :: Namespace
  , _logContext :: LogContexts
  , _logEnv :: LogEnv
  }

makeLenses ''Env

newtype Tabletop m a = Tabletop { unTabletop :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (Monad m) => MonadReader Env (Tabletop m)

instance MonadUnliftIO m => MonadUnliftIO (Tabletop m) where
  askUnliftIO =
    Tabletop $ withUnliftIO $ \u ->
      return (UnliftIO (unliftIO u . unTabletop))

instance (MonadIO m) => Katip (Tabletop m) where
  getLogEnv =
    view logEnv

  localLogEnv f (Tabletop m) =
    Tabletop (local (over logEnv f) m)

instance (MonadIO m) => KatipContext (Tabletop m) where
  getKatipContext =
    view logContext

  localKatipContext f (Tabletop m) =
    Tabletop (local (over logContext f) m)

  getKatipNamespace =
    view logNamespace

  localKatipNamespace f (Tabletop m) =
    Tabletop (local (over logNamespace f) m)
