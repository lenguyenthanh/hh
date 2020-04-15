{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module HH.Cli.Command.CreateTeam
  ( CreateTeamArgs (..),
    createTeamArgsParser,
    runCreateTeam,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text, pack)
import HH.Cli.Command.Internal.Parser
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Github
import HH.Internal.Prelude
import Options.Applicative

data CreateTeamArgs
  = CreateTeamArgs
      { org :: Text,
        teamName :: Text,
        description :: Maybe Text,
        secret :: Bool,
        users :: [Text]
      }
  deriving (Show)

createTeamArgsParser :: Parser CreateTeamArgs
createTeamArgsParser =
  CreateTeamArgs
    <$> organizationParser
    <*> teamNameParser
    <*> optional descriptionParser
    <*> secretParser
    <*> some usersParser

teamNameParser :: Parser Text
teamNameParser =
  strOption
    ( long "name"
        <> short 'n'
        <> metavar "text"
        <> help "Name of the team"
    )

descriptionParser :: Parser Text
descriptionParser =
  strOption
    ( long "description"
        <> short 'd'
        <> metavar "text"
        <> help "Description of the team"
    )

secretParser :: Parser Bool
secretParser =
  switch
    (long "secret" <> short 's' <> help "Flag to set the new team secret")

usersParser :: Parser Text
usersParser =
  option
    str
    (long "users" <> short 'u' <> metavar "list" <> help "List of users for the new team")

runCreateTeam ::
  (MonadReader UserConfig m, MonadConsole m, MonadGithub m) =>
  CreateTeamArgs ->
  m ()
runCreateTeam CreateTeamArgs {org, teamName, description, secret, users} = do
  conf <- ask
  let privacy =
        if secret
          then "secret"
          else "closed"
  let createTeamArgs =
        CreateTeam
          { org = org,
            name = teamName,
            description = description,
            users = users,
            privacy = privacy,
            token = githubToken conf
          }
  result <- runExceptT $ createTeam createTeamArgs
  case result of
    Left e ->
      printLn $ "Failed to create team " <> teamName <> " Because\n" <> (pack . show $ e)
    Right r ->
      printLn $ "Created team " <> teamName <> " successfully at " <> (r ^. #htmlUrl)
