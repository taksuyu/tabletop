{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving,
  TypeFamilies, UndecidableInstances #-}

module Tabletop.Common where

import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Monad.Reader
import Katip
import UnliftIO

import Backhand

import Tabletop.Config
import Tabletop.Message.Ur

newtype Tabletop m a = Tabletop { unTabletop :: ReaderT Env m a }
  deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (Monad m) => MonadReader Env (Tabletop m)

instance MonadUnliftIO m => MonadUnliftIO (Tabletop m) where
  askUnliftIO =
    Tabletop $ withUnliftIO $ \u ->
      return (UnliftIO (unliftIO u . unTabletop))

instance (MonadIO m) => Katip (Tabletop m) where
  getLogEnv =
    asks logEnv

  localLogEnv f (Tabletop m) =
    Tabletop (local (\s -> s { logEnv = f (logEnv s)}) m)

instance (MonadIO m) => KatipContext (Tabletop m) where
  getKatipContext =
    asks logContext

  localKatipContext f (Tabletop m) =
    Tabletop (local (\s -> s { logContext = f (logContext s)}) m)

  getKatipNamespace =
    asks logNamespace

  localKatipNamespace f (Tabletop m) =
    Tabletop (local (\s -> s { logNamespace = f (logNamespace s)}) m)

data Env = Env
  { bhand :: TabletopBackhand
  , config :: TabletopConfig
  , logNamespace :: Namespace
  , logContext :: LogContexts
  , logEnv :: LogEnv
  }

-- FIXME: Use a proper type for the Channels
data TabletopBackhand = TabletopBackhand
  { urChannels :: Channels T.Text (Message UrMessage) UrTabletopResponse
  }
