{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module HH.Cli.Command.CloneRepos
  ( CloneReposArgs (..),
    cloneReposArgsParser,
    runCloneRepos,
  )
where

import Control.Lens
import Control.Monad.Reader
import HH.Cli.Command.Internal.Common
import HH.Cli.Command.Internal.Parser
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Git
import HH.Effect.Github
import HH.Internal.Prelude
import Options.Applicative
import System.FilePath.Extended

data CloneReposArgs
  = CloneReposArgs
      { org :: Text,
        regex :: Maybe Text,
        useHttps :: Bool
      }
  deriving (Show)

cloneReposArgsParser :: Parser CloneReposArgs
cloneReposArgsParser =
  CloneReposArgs
    <$> organizationParser
    <*> optional regexParser
    <*> useHttpsParser

runCloneRepos ::
  (MonadReader UserConfig m, MonadConsole m, MonadGithub m, MonadGit m) =>
  CloneReposArgs ->
  m ()
runCloneRepos CloneReposArgs {org, regex, useHttps} = do
  conf <- ask
  response <- runExceptT $ fetchAndFilterRepos conf org regex
  case response of
    Right repos ->
      mapM_ (cloneRepo useHttps) repos
    Left err ->
      printLn $ "Error " <> (pack . show $ err)

cloneRepo ::
  (MonadReader UserConfig m, MonadGit m, MonadConsole m) =>
  Bool ->
  RemoteRepo ->
  m ()
cloneRepo useHttps repo = do
  conf <- ask
  let path = concatPath [absRootPath conf, nameWithOwner repo]
  success <- clone (pickUrl useHttps repo) path
  if success
    then printLn $ "Cloned " <> repo ^. #name <> " success"
    else printLn $ "Failed to clone " <> repo ^. #name
