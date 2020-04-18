{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module HH.Cli.Command.Internal.Common
  ( filterRepos,
    fetchAndFilterRepos,
  )
where

import Control.Lens
import Control.Monad.Except
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Github
import HH.Internal.Prelude
import Text.Regex.TDFA

filterRepos :: Maybe Text -> [RemoteRepo] -> [RemoteRepo]
filterRepos Nothing x = x
filterRepos (Just regex) xs = filter (\x -> (x ^. #name) =~ regex :: Bool) xs

fetchAndFilterRepos ::
  (MonadConsole m, MonadGithub m) =>
  UserConfig ->
  Text ->
  Maybe Text ->
  ExceptT GQLError m [RemoteRepo]
fetchAndFilterRepos config orgName regex = do
  let ghToken = config ^. #githubToken
  response <- fetchOrgRepos orgName ghToken
  pure $ filterRepos regex response
