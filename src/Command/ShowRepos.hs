{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Command.ShowRepos
    ( ShowRepArgs(..)
    , showRepoArgsParser
    , runShowRepos
    )
  where

import AppConfig
import Command.Internal.Common
import Command.Internal.Parser
import Control.Monad.Reader
import Data.Text (Text)
import Effect.Config
import Effect.Console
import Effect.Github
import Env
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
  let AppConfig{..} = appConfig env
  response <- fetchAndFilterRepos configDir configName org regex
  case response of
    Right repos -> mapM_ showRepo repos
    Left err -> printLn $ "Error " <> err

showRepo :: MonadConsole m => RemoteRepo -> m ()
showRepo repo = printLn $
  "Repo name: " <> name repo <> ", url: " <> url repo
