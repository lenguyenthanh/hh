{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module HH.Effect.FileSystem
    (MonadFileSystem(..)
    )
  where

import Control.Monad.Reader
import Data.Text
import HH.App
import qualified System.Directory as D

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  -- | Reads a file at the given path and returns its contents. If the file does
  -- not exist, is not accessible, or is improperly encoded, this method throws
  -- an exception.
  createDirectoryIfMissing :: Text -> m ()

  default createDirectoryIfMissing :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m ()
  createDirectoryIfMissing = lift . createDirectoryIfMissing

instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
instance MonadFileSystem m => MonadFileSystem (AppM m)

instance MonadFileSystem IO where
  createDirectoryIfMissing = (D.createDirectoryIfMissing True). unpack


