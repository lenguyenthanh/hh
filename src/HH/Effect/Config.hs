{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module HH.Effect.Config
    ( MonadConfig(..)
    , U.UserConfig(..)
    , U.GetConfigError(..)
    )
  where

import Control.Exception.Safe (IOException)
import Control.Monad.Except
import Control.Monad.Reader
import HH.App
import HH.AppConfig
import HH.Internal.Prelude
import qualified HH.UserConfig as U

class Monad m => MonadConfig m where
  getConfig :: AppConfig -> m (Either U.GetConfigError U.UserConfig)
  saveConfig :: AppConfig -> U.UserConfig -> ExceptT IOException m ()

  default getConfig
    :: (MonadTrans t, MonadConfig m', m ~ t m')
    => AppConfig -> m (Either U.GetConfigError U.UserConfig)
  getConfig appConf = lift $ getConfig appConf

  default saveConfig
    :: (MonadTrans t, MonadConfig m', m ~ t m')
    => AppConfig -> U.UserConfig -> ExceptT IOException m ()
  saveConfig conf = ExceptT . lift . runExceptT . saveConfig conf

instance MonadConfig m => MonadConfig (ReaderT r m)
instance MonadConfig m => MonadConfig (AppM e m)

instance MonadConfig IO where
  saveConfig appConf = U.saveConfig appConf
  getConfig = runExceptT. U.getConfig
