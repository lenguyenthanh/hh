{-# LANGUAGE FlexibleContexts #-}

module HH.Cli.Command.CreateBranch
  ( CreateBranchArgs (..),
    createBranchArgsParser,
    runCreateBranch,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text, pack)
import HH.Cli.Command.Internal.Common
import HH.Cli.Command.Internal.Parser
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Git
import HH.Effect.Github
import HH.Internal.Prelude
import Options.Applicative
import System.FilePath.Extended

data CreateBranchArgs
  = CreateBranchArgs
      { org :: Text,
        regex :: Maybe Text,
        newBranch :: Text,
        baseBranch :: Text,
        useHttps :: Bool
      }
  deriving (Show)

createBranchArgsParser :: Parser CreateBranchArgs
createBranchArgsParser =
  CreateBranchArgs
    <$> organizationParser
    <*> optional regexParser
    <*> newBranchParser
    <*> baseBranchParser
    <*> useHttpsParser

runCreateBranch ::
  (MonadReader UserConfig m, MonadConsole m, MonadGithub m, MonadGit m) =>
  CreateBranchArgs ->
  m ()
runCreateBranch CreateBranchArgs {org, regex, newBranch, baseBranch, useHttps} = do
  conf <- ask
  response <- runExceptT $ fetchAndFilterRepos conf org regex
  case response of
    Right repos ->
      mapM_ (mkBranch useHttps newBranch baseBranch) repos
    Left err ->
      printLn $ "Error " <> (pack . show $ err)

mkBranch ::
  (MonadReader UserConfig m, MonadConsole m, MonadGit m) =>
  Bool ->
  Text ->
  Text ->
  RemoteRepo ->
  m ()
mkBranch useHttps newBranch baseBranch repo = do
  conf <- ask
  let path = concatPath [absRootPath conf, nameWithOwner repo]
  isGit <- isGitDir path
  success <- doBranch isGit path >> pushBranch path newBranch
  if success
    then printLn $ "Created " <> newBranch <> " success"
    else printLn $ "Failed to create " <> newBranch
  where
    doBranch isGitDir' path =
      if isGitDir'
        then createNewBranch path newBranch baseBranch
        else
          clone path (pickUrl useHttps repo)
            >> createNewBranch path newBranch baseBranch
