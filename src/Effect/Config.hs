{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module Effect.Config
    ( MonadConfig(..)
    , U.UserConfig(..)
    )
  where


import App
import Control.Monad.Reader
import qualified UserConfig as U


class Monad m => MonadConfig m where
  getConfig :: m U.UserConfig
  saveConfig :: U.UserConfig -> m ()

  default getConfig :: (MonadTrans t, MonadConfig m', m ~ t m') => m U.UserConfig
  getConfig = lift $ getConfig

  default saveConfig :: (MonadTrans t, MonadConfig m', m ~ t m') => U.UserConfig -> m ()
  saveConfig = lift . saveConfig


instance MonadConfig m => MonadConfig (ReaderT r m)
instance MonadConfig m => MonadConfig (AppM m)

instance MonadConfig IO where
  saveConfig = U.saveConfig
  getConfig = U.getConfig

