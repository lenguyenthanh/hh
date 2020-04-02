{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module Effect.Console
    (MonadConsole(..)
    )
  where

import Control.Monad.Reader
import Data.Text
import App

-- | A class of monads that can interact with the Console.
class Monad m => MonadConsole m where
  printLn :: Text -> m ()

  default printLn :: (MonadTrans t, MonadConsole m', m ~ t m') => Text -> m ()
  printLn = lift . printLn

instance MonadConsole m => MonadConsole (ReaderT r m)
instance MonadConsole m => MonadConsole (AppM m)

instance MonadConsole IO where
  printLn = putStrLn . unpack
