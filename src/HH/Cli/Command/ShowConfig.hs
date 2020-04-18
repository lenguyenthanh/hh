{-# LANGUAGE FlexibleContexts #-}

module HH.Cli.Command.ShowConfig where

import Control.Monad.Reader
import HH.Effect.Config
import HH.Effect.Console
import HH.Internal.Prelude

runShowConfig ::
  (MonadReader UserConfig m, MonadConsole m) =>
  m ()
runShowConfig = do
  conf <- ask
  printLn . pack $ show conf
