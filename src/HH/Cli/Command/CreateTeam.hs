{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HH.Cli.Command.CreateTeam
    ( CreateTeamArgs(..)
    , createTeamArgsParser
    , runCreateTeam
    )
  where

import Control.Monad.Reader
import Data.Text (Text, pack)
import HH.Cli.Command.Internal.Parser
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Github
import HH.Env
import Options.Applicative

data CreateTeamArgs
  = CreateTeamArgs
    { org :: Text
    , teamName :: Text
    , description :: Maybe Text
    , secret :: Bool
    , users :: [Text]
    }
  deriving (Show)

createTeamArgsParser :: Parser CreateTeamArgs
createTeamArgsParser = CreateTeamArgs
               <$> organizationParser
               <*> teamNameParser
               <*> optional descriptionParser
               <*> secretParser
               <*> some usersParser

teamNameParser :: Parser Text
teamNameParser = strOption
    (long "name"
    <> short 'n'
    <> metavar "text"
    <> help "Name of the team")

descriptionParser :: Parser Text
descriptionParser = strOption
    (long "description"
    <> short 'd'
    <> metavar "text"
    <> help "Description of the team")

secretParser :: Parser Bool
secretParser = switch
    (long "secret" <> short 's' <> help "Flag to set the new team secret")

usersParser :: Parser Text
usersParser = option str
    (long "users" <> short 'u' <> metavar "list" <> help "List of users for the new team")

runCreateTeam
  :: (MonadReader Env m, MonadConsole m, MonadConfig m, MonadGithub m)
  => CreateTeamArgs -> m ()
runCreateTeam (CreateTeamArgs {..}) = do
  env <- ask
  let conf = appConfig env
  userConfig <- getConfig conf
  let privacy = "secret"
  let createTeamArgs = CreateTeam { createTeamOrg = org
                                  , createTeamName = teamName
                                  , createTeamDescription = description
                                  , createTeamUsers = users
                                  , createTeamPrivacy = privacy
                                  , createTeamToken = githubToken userConfig
                                  }
  response <- createTeam createTeamArgs
  printLn.pack $ show response
