{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module HH.Effect.Github
    ( MonadGithub(..)
    , G.RemoteRepo(..)
    , G.CreateTeam(..)
    , G.CreateTeamResponse(..)
    , G.httpsUrl
    )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text
import HH.App
import qualified HH.Github.Api as G

class Monad m => MonadGithub m where
  fetchUsername :: Text -> ExceptT Text m Text
  createTeam :: G.CreateTeam -> m G.CreateTeamResponse
  fetchOrgRepos :: Text -> Text -> m (Either Text [G.RemoteRepo])

  default fetchUsername :: (MonadTrans t, MonadGithub m', m ~ t m') => Text -> ExceptT Text m Text
  fetchUsername = ExceptT . lift . runExceptT . fetchUsername

  default createTeam :: (MonadTrans t, MonadGithub m', m ~ t m') => G.CreateTeam -> m G.CreateTeamResponse
  createTeam = lift.createTeam

  default fetchOrgRepos :: (MonadTrans t, MonadGithub m', m ~ t m') => Text -> Text -> m (Either Text [G.RemoteRepo])
  fetchOrgRepos org token = lift $ fetchOrgRepos org token

instance MonadGithub m => MonadGithub (ReaderT r m)
instance MonadGithub m => MonadGithub (AppM e m)

instance MonadGithub IO where
  fetchUsername = G.fetchUsername
  fetchOrgRepos = G.fetchOrgRepos
  createTeam = G.createTeam

