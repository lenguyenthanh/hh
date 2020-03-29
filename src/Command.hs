{-# LANGUAGE OverloadedStrings #-}

module Command
    (Command(..)
    , commands
    )
  where

import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Text
import Options.Applicative
import qualified Path as P
import UserConfig

data Command =
    ShowConfig
  | Init { root :: Text
         , token :: Text
         }
  | ShowRepo { org :: Text }
    deriving (Show)

commands :: Parser Command
commands = hsubparser
    (  command
          "init"
          (info initCommand
                (progDesc "Init your configuration")
          )
    <> command
          "show-config"
          (info (pure ShowConfig)
                (progDesc "Show your configuration")
          )

    <> command
          "show-repos"
          (info showRepoCommand
                (progDesc "Show all repos in an organization")
          )
    )

showRepoCommand :: Parser Command
showRepoCommand = ShowRepo
               <$> strOption
                (long "org"
                <> short 'o'
                <> metavar "name"
                <> help "Organization name")

initCommand :: Parser Command
initCommand = Init
          <$> option parseRoot
            (long "root"
            <> short 'r'
            <> metavar "dir"
            <> help "Root directory for all repositories")
          <*> strOption
            (long "token"
            <> short 't'
            <> metavar "Github token"
            <> help "We need your github token to query github api")

parseRoot :: ReadM Text
parseRoot = fmap pack $ eitherReader parseAbsDir

parseAbsDir :: FilePath -> Either String String
parseAbsDir f = bimap show P.fromAbsDir $ P.parseAbsDir f

