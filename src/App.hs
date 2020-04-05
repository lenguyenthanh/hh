{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
    (AppM(..)
    , runAppM
    )
  where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Env

newtype AppM m a =
    AppM { unAppM :: ReaderT Env m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Env)

runAppM :: Env -> AppM m a -> m a
runAppM env app = runReaderT (unAppM app) env
