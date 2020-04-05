{-# LANGUAGE FlexibleContexts #-}

module HH
    (hh
    )
  where

import App
import AppConfig
import Command
import Command.CloneRepos
import Command.Create
import Command.InitConfig
import Command.ShowRepos
import Control.Monad.Reader
import Effect.Command
import Effect.Config
import Effect.Console
import Effect.FileSystem
import Effect.Git
import Effect.Github
import Env

hh :: IO ()
hh = do
  conf <- getAppConfig
  let env = Env { appConfig = conf }
  runAppM env main

main
  :: (MonadConfig m, MonadConsole m, MonadGithub m, MonadGit m, MonadCommand m, MonadFileSystem m)
  => AppM m ()
main = do
  command <- getCommand
  runCommand command

runCommand
  :: (MonadConfig m, MonadConsole m, MonadGithub m, MonadGit m, MonadFileSystem m, MonadReader Env m)
  => Command -> m ()
runCommand (Init args) = runSaveConfig args
runCommand ShowConfig = runShowConfig
runCommand (ShowRepos args) = runShowRepos args
runCommand (CloneRepos args) = runCloneRepos args
runCommand (Create args) = runCreate args
