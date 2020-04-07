{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HH.Cli.Command.ShowRepos
    ( ShowRepArgs(..)
    , showRepoArgsParser
    , runShowRepos
    )
  where

import Control.Monad.Reader
import Data.Text (Text)
import HH.Cli.Command.Internal.Common
import HH.Cli.Command.Internal.Parser
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Github
import HH.Env
import Options.Applicative

data ShowRepArgs
  = ShowRepArgs
    { org :: Text
    , regex :: Maybe Text
    }
  deriving (Show)

showRepoArgsParser :: Parser ShowRepArgs
showRepoArgsParser = ShowRepArgs
               <$> organizationParser
               <*> optional regexParser

runShowRepos
  :: (MonadReader Env m, MonadConfig m, MonadConsole m, MonadGithub m)
  => ShowRepArgs -> m ()
runShowRepos (ShowRepArgs {..}) = do
  env <- ask
  let conf = appConfig env
  response <- fetchAndFilterRepos conf org regex
  case response of
    Right repos -> mapM_ showRepo repos
    Left err -> printLn $ "Error " <> err

showRepo :: MonadConsole m => RemoteRepo -> m ()
showRepo repo = printLn $
  "Repo name: " <> name repo <> ", url: " <> url repo
