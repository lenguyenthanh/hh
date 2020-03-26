{-# LANGUAGE OverloadedStrings #-}

module Command
    (Command(..)
    , UserConfig(..)
    , commands
    )
  where

import UserConfig
import Data.Aeson
import Options.Applicative

data Command = Init { config :: UserConfig }
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
    -- <**> versionOption <**> helper

initCommand :: Parser Command
initCommand = Init <$> userConfig

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("hh version 0.1.0")
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)
