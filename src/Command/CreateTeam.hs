{-# LANGUAGE RecordWildCards #-}

module Command.CreateTeam
    ( CreateTeamArgs(..)
    , createTeamArgsParser
    , runCreateTeam
    )
  where

import Command.Internal.Parser
import Data.Text (Text, pack)
import Effect.Config
import Effect.Console
import Effect.Github
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
  :: (MonadConsole m, MonadConfig m, MonadGithub m)
  => CreateTeamArgs -> m ()
runCreateTeam (CreateTeamArgs {..}) = do
  conf <- getConfig
  let privacy = "secret"
  let createTeamArgs = CreateTeam { createTeamOrg = org
                                  , createTeamName = teamName
                                  , createTeamDescription = description
                                  , createTeamUsers = users
                                  , createTeamPrivacy = privacy
                                  , createTeamToken = githubToken conf
                                  }
  response <- createTeam createTeamArgs
  printLn.pack $ show response
