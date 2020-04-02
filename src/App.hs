{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
    (AppM(..)
    , runAppM
    )
  where


import Control.Monad.IO.Class
import Control.Monad.Reader

newtype AppM m a =
    AppM { unAppM :: ReaderT () m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader ())

runAppM :: AppM m a -> () -> m a
runAppM app env = runReaderT (unAppM app) env
