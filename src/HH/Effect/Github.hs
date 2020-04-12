{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}

module HH.Effect.Github
    ( MonadGithub(..)
    , G.RemoteRepo(..)
    , G.CreateTeam(..)
    , G.CreateTeamResponse(..)
    , G.pickUrl
    , G.GQLError
    )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text
import HH.App
import qualified HH.Github.Api as G
import HH.Internal.Prelude
import Network.HTTP.Req

class Monad m => MonadGithub m where
  fetchUsername :: Text -> ExceptT G.GQLError m Text
  createTeam :: G.CreateTeam -> ExceptT HttpException m G.CreateTeamResponse
  fetchOrgRepos :: Text -> Text -> ExceptT G.GQLError m [G.RemoteRepo]

  default fetchUsername
    :: (MonadTrans t, MonadGithub m', m ~ t m')
    => Text -> ExceptT G.GQLError m Text
  fetchUsername = ExceptT . lift . runExceptT . fetchUsername

  default createTeam
    :: (MonadTrans t, MonadGithub m', m ~ t m')
    => G.CreateTeam -> ExceptT HttpException m G.CreateTeamResponse
  createTeam = ExceptT . lift . runExceptT . createTeam

  default fetchOrgRepos
    :: (MonadTrans t, MonadGithub m', m ~ t m')
    => Text -> Text -> ExceptT G.GQLError m [G.RemoteRepo]
  fetchOrgRepos org = ExceptT . lift . runExceptT . fetchOrgRepos org

instance MonadGithub m => MonadGithub (ReaderT r m)
instance MonadGithub m => MonadGithub (AppM e m)

instance MonadGithub IO where
  fetchUsername = G.fetchUsername
  fetchOrgRepos = G.fetchOrgRepos
  createTeam = G.createTeam
