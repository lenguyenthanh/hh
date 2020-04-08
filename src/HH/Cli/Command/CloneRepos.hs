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
import HH.Cli.Command.Internal.Common
import HH.Cli.Command.Internal.Parser
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Git
import HH.Effect.Github
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
  :: (MonadReader UserConfig m, MonadConsole m, MonadGithub m, MonadGit m)
  => CloneReposArgs -> m ()
runCloneRepos (CloneReposArgs {..}) = do
  conf <- ask
  response <- fetchAndFilterRepos conf org regex
  case response of
    Right repos ->
      mapM_ (cloneRepo useHttps) repos
    Left err ->
      printLn $ "Error " <> err

cloneRepo
  :: (MonadReader UserConfig m, MonadGit m, MonadConsole m)
  => Bool -> RemoteRepo -> m ()
cloneRepo useHttps repo = do
  conf <- ask
  let path = concatPath [absRootPath conf, nameWithOwner repo]
  _ <- clone url path
  printLn $ "Cloned " <> name repo <> " success"
  where
    url = if useHttps
            then httpsUrl repo
            else sshUrl repo
