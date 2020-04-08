{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module HH.Effect.Config
    ( MonadConfig(..)
    , U.UserConfig(..)
    )
  where

import Control.Monad.Reader
import HH.App
import HH.AppConfig
import qualified HH.UserConfig as U

class Monad m => MonadConfig m where
  getConfig :: AppConfig -> m U.UserConfig
  saveConfig :: AppConfig -> U.UserConfig -> m ()

  default getConfig
    :: (MonadTrans t, MonadConfig m', m ~ t m')
    => AppConfig -> m U.UserConfig
  getConfig conf = lift $ getConfig conf

  default saveConfig
    :: (MonadTrans t, MonadConfig m', m ~ t m')
    => AppConfig -> U.UserConfig -> m ()
  saveConfig conf = lift . saveConfig conf

instance MonadConfig m => MonadConfig (ReaderT r m)
instance MonadConfig m => MonadConfig (AppM e m)

instance MonadConfig IO where
  saveConfig = U.saveConfig
  getConfig = U.getConfig
