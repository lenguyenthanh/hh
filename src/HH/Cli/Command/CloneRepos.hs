{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HH.Cli.Command.CloneRepos
    ( CloneReposArgs(..)
    , cloneReposArgsParser
    , runCloneRepos
    )
  where

import Control.Monad.Reader
import Data.Text (Text)
import HH.AppConfig
import HH.Cli.Command.Internal.Common
import HH.Cli.Command.Internal.Parser
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Git
import HH.Effect.Github
import HH.Env
import Options.Applicative

data CloneReposArgs
  = CloneReposArgs
    { org :: Text
    , regex :: Maybe Text
    , useHttps :: Bool
    }
  deriving (Show)

cloneReposArgsParser :: Parser CloneReposArgs
cloneReposArgsParser = CloneReposArgs
               <$> organizationParser
               <*> optional regexParser
               <*> useHttpsParser

runCloneRepos
  :: (MonadReader Env m, MonadConfig m, MonadConsole m, MonadGithub m, MonadGit m)
  => CloneReposArgs -> m ()
runCloneRepos (CloneReposArgs {..}) = do
  env <- ask
  let AppConfig{..} = appConfig env
  response <- fetchAndFilterRepos configDir configName org regex
  case response of
    Right repos ->
      mapM_ (cloneRepo useHttps) repos
    Left err ->
      printLn $ "Error " <> err

cloneRepo
  :: (MonadReader Env m, MonadConfig m, MonadGit m, MonadConsole m)
  => Bool -> RemoteRepo -> m ()
cloneRepo useHttps repo = do
  env <- ask
  let AppConfig{..} = appConfig env
  conf <- getConfig configDir configName
  let path = concatPath [absRootPath conf, nameWithOwner repo]
  _ <- clone url path
  printLn $ "Cloned " <> name repo <> " success"
  where
    url = if useHttps
            then httpsUrl repo
            else sshUrl repo
