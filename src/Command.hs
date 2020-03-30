{-# LANGUAGE OverloadedStrings #-}

module Command
    (Command(..)
    , commands
    )
  where

import Command.InitConfig
import Command.ShowRepo
import Options.Applicative

data Command =
    ShowConfig
  | Init InitArgs
  | ShowRepo ShowRepArgs
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
showRepoCommand = ShowRepo <$> showRepoArgsParser

initCommand :: Parser Command
initCommand = Init <$> initArgsParser

