{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Command.CreateBranch
    ( CreateBranchArgs(..)
    , createBranchArgsParser
    , runCreateBranch
    )
  where

import AppConfig
import Command.Internal.Common
import Command.Internal.Parser
import Control.Monad.Reader
import Data.Text (Text)
import Effect.Config
import Effect.Console
import Effect.Git
import Effect.Github
import Env
import Options.Applicative

data CreateBranchArgs
  = CreateBranchArgs
    { org :: Text
    , regex :: Maybe Text
    , newBranch :: Text
    , baseBranch :: Text
    , useHttps :: Bool
    }
  deriving (Show)

createBranchArgsParser :: Parser CreateBranchArgs
createBranchArgsParser = CreateBranchArgs
               <$> organizationParser
               <*> optional regexParser
               <*> newBranchParser
               <*> baseBranchParser
               <*> useHttpsParser

runCreateBranch
  :: (MonadReader Env m, MonadConfig m, MonadConsole m, MonadGithub m, MonadGit m)
  => CreateBranchArgs -> m ()
runCreateBranch (CreateBranchArgs {..}) = do
  env <- ask
  let AppConfig{..} = appConfig env
  response <- fetchAndFilterRepos configDir configName org regex
  case response of
    Right repos -> mapM_ (mkBranch useHttps newBranch baseBranch) repos
    Left err -> printLn $ "Error " <> err

mkBranch
  :: (MonadReader Env m, MonadConfig m, MonadConsole m, MonadGit m)
  => Bool -> Text -> Text -> RemoteRepo -> m ()
mkBranch useHttps newBranch baseBranch repo = do
  env <- ask
  let AppConfig{..} = appConfig env
  conf <- getConfig configDir configName
  let path = concatPath [absRootPath conf, nameWithOwner repo]
  isGit <- isGitDir path
  doBranch isGit path >> pushBranch path newBranch
  where
    url = if useHttps
            then httpsUrl repo
            else sshUrl repo
    mkBranch' path = createNewBranch path newBranch baseBranch
    doBranch isGitDir path = if isGitDir
                              then mkBranch' path
                              else clone path url >> mkBranch' path
