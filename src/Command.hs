{-# LANGUAGE OverloadedStrings #-}

module Command
    (Command(..)
    , commands
    )
  where

import Data.Bifunctor (bimap)
import Data.Text
import Options.Applicative
import qualified Path as P

data Command =
    ShowConfig
  | Init { root :: Text
         , token :: Text
         }
  | ShowRepo { org :: Text
             , regex :: Maybe Text
             }
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
               <$> organizationOption
               <*> optional regexOption

initCommand :: Parser Command
initCommand = Init
          <$> rootOption
          <*> tokenOption

rootOption :: Parser Text
rootOption = option parseRoot
    (long "root" <> short 'r' <> metavar "<dir>" <> help "Root directory for all repositories")

tokenOption :: Parser Text
tokenOption = strOption
    (long "token" <> short 't' <> metavar "Github token" <> help "We need your github token to query github api")

organizationOption :: Parser Text
organizationOption = strOption
    (long "org" <> short 'o' <> metavar "name" <> help "Organization name")

regexOption :: Parser Text
regexOption = strOption
    (long "regex" <> short 'r' <> metavar "regular expression" <> help "Regular expression to filter repositories")

parseRoot :: ReadM Text
parseRoot = fmap pack $ eitherReader parseAbsDir

parseAbsDir :: FilePath -> Either String String
parseAbsDir f = bimap show P.fromAbsDir $ P.parseAbsDir f
