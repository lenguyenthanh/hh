{-# LANGUAGE RecordWildCards #-}

module Command.CloneRepos
    ( CloneReposArgs(..)
    , cloneReposArgsParser
    , runCloneRepos
    )
  where

import Command.Internal.Common
import Command.Internal.Parser
import Data.Text (Text)
import Effect.Config
import Effect.Console
import Effect.Git
import Effect.Github
import Options.Applicative

data CloneReposArgs =
  CloneReposArgs { org :: Text
                 , regex :: Maybe Text
                 , useHttps :: Bool
                 } deriving (Show)

cloneReposArgsParser :: Parser CloneReposArgs
cloneReposArgsParser = CloneReposArgs
               <$> organizationParser
               <*> optional regexParser
               <*> useHttpsParser

runCloneRepos :: (MonadConfig m, MonadConsole m, MonadGithub m, MonadGit m) => CloneReposArgs -> m ()
runCloneRepos (CloneReposArgs {..}) = do
  response <- fetchAndFilterRepos org regex
  case response of
    Right repos -> mapM_ (cloneRepo useHttps) repos
    Left err -> printLn $ "Error " <> err

cloneRepo :: (MonadConfig m, MonadGit m, MonadConsole m) => Bool -> RemoteRepo -> m ()
cloneRepo useHttps repo = do
  conf <- getConfig
  let path = concatPath [absRootPath conf, nameWithOwner repo]
  _ <- clone url path
  printLn $ "Cloned " <> name repo <> " success"
  where
    url = if useHttps
            then httpsUrl repo
            else sshUrl repo
