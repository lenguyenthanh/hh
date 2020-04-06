{-# LANGUAGE FlexibleContexts #-}
module HH.Cli.Cli where

import Control.Monad.Reader
import HH.Cli.Command
import HH.Cli.Command.CloneRepos
import HH.Cli.Command.Create
import HH.Cli.Command.InitConfig
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
runCommand (Init args) = runSaveConfig args
runCommand ShowConfig = runShowConfig
runCommand (ShowRepos args) = runShowRepos args
runCommand (CloneRepos args) = runCloneRepos args
runCommand (Create args) = runCreate args
