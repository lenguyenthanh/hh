{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module HH.Effect.Command
  ( MonadCommand (..),
  )
where

import Control.Monad.Reader
import HH.App
import HH.Cli.Command
import HH.Internal.Prelude

-- | A class of monads that can access command-line arguments.
class Monad m => MonadCommand m where
  -- | Returns the command-line interface provided to the program.
  getCommand :: m Command
  default getCommand :: (MonadTrans t, MonadCommand m', m ~ t m') => m Command
  getCommand = lift getCommand

instance MonadCommand m => MonadCommand (ReaderT r m)

instance MonadCommand m => MonadCommand (AppM e m)

instance MonadCommand IO where
  getCommand = commands
