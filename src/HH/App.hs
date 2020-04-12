{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HH.App
    (AppM(..)
    , runAppM
    )
  where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import HH.Internal.Prelude

newtype AppM r m a =
    AppM { unAppM :: ReaderT r m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader r, MonadThrow)

runAppM :: r -> AppM r m a -> m a
runAppM env app = runReaderT (unAppM app) env
