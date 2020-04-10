{-# LANGUAGE FlexibleContexts #-}

module HH.Main
    (mainIO
    )
  where

import Control.Exception.Safe
import HH.App
import HH.AppConfig
import HH.Cli.Cli
import HH.Effect.Command
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.FileSystem
import HH.Effect.Git
import HH.Effect.Github
import HH.Env

mainIO :: IO ()
mainIO = do
  conf <- getAppConfig
  let env = Env { appConfig = conf }
  runAppM env main

main
  :: (MonadConfig m, MonadConsole m, MonadGithub m
    , MonadGit m, MonadCommand m, MonadFileSystem m
    , MonadThrow m)
  => AppM Env m ()
main = do
  command <- getCommand
  runCommand command
