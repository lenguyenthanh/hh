{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module HH.Effect.FileSystem
  ( MonadFileSystem (..),
  )
where

import Control.Error
import Control.Exception.Safe (IOException)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text
import HH.App
import HH.Internal.Prelude
import qualified System.Directory as D

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  -- | Reads a file at the given path and returns its contents. If the file does
  -- not exist, is not accessible, or is improperly encoded, this method throws
  -- an exception.
  createDirectoryIfMissing :: Text -> ExceptT IOException m ()
  default createDirectoryIfMissing ::
    (MonadTrans t, MonadFileSystem m', m ~ t m') =>
    Text ->
    ExceptT IOException m ()
  createDirectoryIfMissing = ExceptT . lift . runExceptT . createDirectoryIfMissing

instance MonadFileSystem m => MonadFileSystem (ReaderT r m)

instance MonadFileSystem m => MonadFileSystem (AppM e m)

instance MonadFileSystem IO where
  createDirectoryIfMissing = tryIO . D.createDirectoryIfMissing True . unpack
