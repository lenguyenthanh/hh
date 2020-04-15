{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module HH.Cli.Command.ShowRepos
  ( ShowReposArgs (..),
    showRepoArgsParser,
    runShowRepos,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text, pack)
import HH.Cli.Command.Internal.Common
import HH.Cli.Command.Internal.Parser
import HH.Effect.Config (UserConfig)
import HH.Effect.Console
import HH.Effect.Github
import HH.Internal.Prelude
import Options.Applicative

data ShowReposArgs
  = ShowReposArgs
      { org :: Text,
        regex :: Maybe Text
      }
  deriving (Show)

showRepoArgsParser :: Parser ShowReposArgs
showRepoArgsParser =
  ShowReposArgs
    <$> organizationParser
    <*> optional regexParser

runShowRepos ::
  (MonadReader UserConfig m, MonadConsole m, MonadGithub m) =>
  ShowReposArgs ->
  m ()
runShowRepos ShowReposArgs {..} = do
  conf <- ask
  response <- runExceptT $ fetchAndFilterRepos conf org regex
  case response of
    Right repos ->
      mapM_ showRepo repos
    Left err ->
      printLn $ "Error " <> (pack . show $ err)

showRepo :: MonadConsole m => RemoteRepo -> m ()
showRepo repo =
  printLn $
    "Repo name: " <> repo ^. #name <> ", url: " <> repo ^. #url
