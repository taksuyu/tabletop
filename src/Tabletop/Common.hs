{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

module Tabletop.Common where

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Katip as K

import qualified Data.Text as T

import Backhand as B

import Tabletop.Message.Ur

newtype Tabletop m a = Tabletop { unTabletop :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (Monad m) => MonadReader Env (Tabletop m)

instance (MonadIO m) => K.Katip (Tabletop m) where
  getLogEnv = asks logEnv

  localLogEnv f (Tabletop m) = Tabletop (local (\s -> s { logEnv = f (logEnv s)}) m)

instance (MonadIO m) => K.KatipContext (Tabletop m) where
  getKatipContext = asks logContext

  localKatipContext f (Tabletop m) = Tabletop (local (\s -> s { logContext = f (logContext s)}) m)

  getKatipNamespace = asks logNamespace

  localKatipNamespace f (Tabletop m) = Tabletop (local (\s -> s { logNamespace = f (logNamespace s)}) m)

deriving instance MonadTrans Tabletop

instance MonadTransControl Tabletop where
  type StT Tabletop a = StT (ReaderT Env) a

  liftWith = defaultLiftWith Tabletop unTabletop

  restoreT = defaultRestoreT Tabletop

deriving instance MonadBase b m => MonadBase b (Tabletop m)

instance MonadBaseControl b m => MonadBaseControl b (Tabletop m) where
  type StM (Tabletop m) a = ComposeSt Tabletop m a

  liftBaseWith = defaultLiftBaseWith

  restoreM = defaultRestoreM


data Env = Env
  { bhand :: TabletopBackhand
  , logNamespace :: K.Namespace
  , logContext :: K.LogContexts
  , logEnv :: K.LogEnv
  }

-- FIXME: Use a proper type for the Channels
data TabletopBackhand = TabletopBackhand
  { urChannelMap :: Channels T.Text (B.Message UrMessage) UrTabletopResponse
  -- , chessChannelMap :: ChannelMap ChessControlMessage
  }
