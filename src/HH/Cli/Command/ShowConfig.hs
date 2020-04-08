{-# LANGUAGE FlexibleContexts #-}

module HH.Cli.Command.ShowConfig where

import Control.Monad.Reader
import Data.Text (pack)
import HH.Effect.Config
import HH.Effect.Console

runShowConfig
  :: (MonadReader UserConfig m, MonadConsole m)
  => m ()
runShowConfig = do
  conf <- ask
  printLn.pack $ show conf
