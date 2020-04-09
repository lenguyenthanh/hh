{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module HH.Effect.Git
    ( MonadGit(..)
    )
  where


import Control.Monad.Reader
import Data.Text
import HH.App
import qualified HH.Git.Git as G

class Monad m => MonadGit m where
  clone :: Text -> Text -> m Bool
  createNewBranch :: Text -> Text -> Text -> m Bool
  isGitDir :: Text -> m Bool
  pushBranch :: Text -> Text -> m Bool

  default clone :: (MonadTrans t, MonadGit m', m ~ t m') => Text -> Text -> m Bool
  clone url path = lift $ clone url path

  default createNewBranch :: (MonadTrans t, MonadGit m', m ~ t m') => Text -> Text -> Text -> m Bool
  createNewBranch path new base = lift $ createNewBranch path new base

  default isGitDir :: (MonadTrans t, MonadGit m', m ~ t m') => Text -> m Bool
  isGitDir path = lift $ isGitDir path

  default pushBranch :: (MonadTrans t, MonadGit m', m ~ t m') => Text -> Text -> m Bool
  pushBranch path branch = lift $ pushBranch path branch

instance MonadGit m => MonadGit (ReaderT r m)
instance MonadGit m => MonadGit (AppM e m)

instance MonadGit IO where
  clone = G.clone
  createNewBranch = G.newBranch
  isGitDir = G.isGitDir
  pushBranch = G.pushBranch
