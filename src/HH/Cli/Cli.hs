{-# LANGUAGE FlexibleContexts #-}
module HH.Cli.Cli where

import Control.Monad.Reader
import HH.App
import HH.Cli.Command
import HH.Cli.Command.CloneRepos
import HH.Cli.Command.Create
import HH.Cli.Command.InitConfig
import HH.Cli.Command.ShowConfig
import HH.Cli.Command.ShowRepos
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.FileSystem
import HH.Effect.Git
import HH.Effect.Github
import HH.Env

runCommand
  :: (MonadConfig m, MonadConsole m, MonadGithub m
     , MonadGit m, MonadFileSystem m, MonadReader Env m)
  => Command -> m ()
runCommand (CloneRepos args) = embedConfig $ runCloneRepos args
runCommand (Create args) = embedConfig $ runCreate args
runCommand (Init args) = runSaveConfig args
runCommand ShowConfig = embedConfig runShowConfig
runCommand (ShowRepos args) = embedConfig $ runShowRepos args

userConfig :: (MonadConfig m) => Env -> m UserConfig
userConfig = getConfig . appConfig

embedConfig :: (MonadConfig m, MonadReader Env m) => AppM UserConfig m () -> m ()
embedConfig n = do
  env <- ask
  conf <- userConfig env
  runAppM conf n
