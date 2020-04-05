{-# LANGUAGE RecordWildCards #-}

module Command.ShowRepos
    ( ShowRepArgs(..)
    , showRepoArgsParser
    , runShowRepos
    )
  where

import Command.Internal.Common
import Command.Internal.Parser
import Data.Text (Text)
import Effect.Config
import Effect.Console
import Effect.Github
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
  :: (MonadConfig m, MonadConsole m, MonadGithub m)
  => ShowRepArgs -> m ()
runShowRepos (ShowRepArgs {..}) = do
  response <- fetchAndFilterRepos org regex
  case response of
    Right repos -> mapM_ showRepo repos
    Left err -> printLn $ "Error " <> err

showRepo
  :: (MonadConsole m)
  => RemoteRepo -> m ()
showRepo repo = printLn $
  "Repo name: " <> name repo <> ", url: " <> url repo
