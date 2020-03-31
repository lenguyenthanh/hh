{-# LANGUAGE RecordWildCards #-}

module Command.CreateTeam
    ( CreateTeamArgs(..)
    , createTeamArgsParser
    , createTeam
    )
  where

import Control.Lens
import Control.Monad (guard)
import Command.Internal.Common
import Command.Internal.Parser
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import qualified Git as G
import qualified Github.Api as Api
import Options.Applicative
import Prelude hiding (putStrLn)
import UserConfig

data CreateTeamArgs =
  CreateTeamArgs { org :: Text
                 , teamName :: Text
                 , description :: Maybe Text
                 , secret :: Bool
                 , users :: [Text]
                 } deriving (Show)

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

createTeam :: CreateTeamArgs -> IO ()
createTeam (CreateTeamArgs org name des secret users) = do
  conf <- getConfig
  let privacy = "secret"
  response <- Api.createTeam org name des users privacy $ conf^.githubToken
  print $ show response
