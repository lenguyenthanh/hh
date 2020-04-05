{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module Effect.Command
    (MonadCommand(..)
    )
  where

import App
import Command
import Control.Monad.Reader


-- | A class of monads that can access command-line arguments.
class Monad m => MonadCommand m where
  -- | Returns the command-line interface provided to the program.
  getCommand :: m Command

  default getCommand :: (MonadTrans t, MonadCommand m', m ~ t m') => m Command
  getCommand = lift getCommand

instance MonadCommand m => MonadCommand (ReaderT r m)
instance MonadCommand m => MonadCommand (AppM m)

instance MonadCommand IO where
  getCommand = commands
