{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}

module HH.Cli.Cli where

import Control.Exception.Safe
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
     , MonadGit m, MonadFileSystem m, MonadReader Env m
     , MonadThrow m)
  => Command -> m ()
runCommand (CloneRepos args) = embedConfig $ runCloneRepos args
runCommand (Create args) = embedConfig $ runCreate args
runCommand (Init args) = runSaveConfig args
runCommand ShowConfig = embedConfig runShowConfig
runCommand (ShowRepos args) = embedConfig $ runShowRepos args

embedConfig :: (MonadThrow m, MonadConsole m, MonadConfig m, MonadReader Env m) => AppM UserConfig m () -> m ()
embedConfig n = do
  env <- ask
  conf <- userConfig env
  runAppM conf n

userConfig :: (MonadThrow m, MonadConsole m, MonadConfig m) => Env -> m UserConfig
userConfig env = do
  result <- getConfig $ appConfig env
  case result of
    Right conf ->
      pure conf
    Left err ->
      throwString $ getUserConfError err

getUserConfError :: GetConfigError -> String
getUserConfError (IOEx ex) = "Failed to read config file" <> show ex
getUserConfError (DecodeError err) = "Failed to read the config file. Format of config file is bad." <> err
getUserConfError ConfigFileNotExist = "Failed to read the config file becaues config file is not exist. You may need to init it with init command first."
