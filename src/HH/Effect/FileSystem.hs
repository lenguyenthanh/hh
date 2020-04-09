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
import Control.Exception.Safe

-- | A class of monads that can interact with the filesystem.
class Monad m => MonadFileSystem m where
  -- | Reads a file at the given path and returns its contents. If the file does
  -- not exist, is not accessible, or is improperly encoded, this method throws
  -- an exception.
  createDirectoryIfMissing :: Text -> m (Either IOException ())

  default createDirectoryIfMissing :: (MonadTrans t, MonadFileSystem m', m ~ t m') => Text -> m (Either IOException ())
  createDirectoryIfMissing = lift . createDirectoryIfMissing

instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
instance MonadFileSystem m => MonadFileSystem (AppM e m)

instance MonadFileSystem IO where
  createDirectoryIfMissing = safeCreateDirectory

safeCreateDirectory :: Text -> IO (Either IOException ())
safeCreateDirectory dirPath = (createDirectory dirPath) `catchIO` \e -> pure $ Left e

createDirectory :: Text -> IO (Either IOException ())
createDirectory dirPath = D.createDirectoryIfMissing True (unpack dirPath) >> (pure $ Right ())
