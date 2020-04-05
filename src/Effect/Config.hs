{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module Effect.Config
    ( MonadConfig(..)
    , U.UserConfig(..)
    )
  where

import App
import Control.Monad.Reader
import Data.Text
import qualified UserConfig as U

class Monad m => MonadConfig m where
  getConfig :: Text -> Text -> m U.UserConfig
  saveConfig :: Text -> Text -> U.UserConfig -> m ()

  default getConfig
    :: (MonadTrans t, MonadConfig m', m ~ t m')
    => Text -> Text -> m U.UserConfig
  getConfig dir name = lift $ getConfig dir name

  default saveConfig
    :: (MonadTrans t, MonadConfig m', m ~ t m')
    => Text -> Text -> U.UserConfig -> m ()
  saveConfig dir name = lift . saveConfig dir name

instance MonadConfig m => MonadConfig (ReaderT r m)
instance MonadConfig m => MonadConfig (AppM m)

instance MonadConfig IO where
  saveConfig = U.saveConfig
  getConfig = U.getConfig
