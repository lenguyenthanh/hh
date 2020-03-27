{-# LANGUAGE OverloadedStrings #-}

module Command
    (Command(..)
    , commands
    )
  where

import Data.Aeson
import Options.Applicative
import qualified Path as P
import UserConfig
import Data.Bifunctor (bimap)

data Command = Init {
                    absRootPath :: String
                    , githubToken :: String }
             | ShowConfig
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
    )

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

parseRoot :: ReadM String
parseRoot = eitherReader parseAbsDir

parseAbsDir :: FilePath -> Either String String
parseAbsDir f = bimap show P.fromAbsDir $ P.parseAbsDir f

