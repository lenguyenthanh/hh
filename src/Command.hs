{-# LANGUAGE OverloadedStrings #-}

module Command
    (Command(..)
    , commands
    )
  where

import Command.CloneRepos
import Command.InitConfig
import Command.ShowRepos
import Options.Applicative
import Command.CreateBranch

data Command =
    ShowConfig
  | Init InitArgs
  | ShowRepos ShowRepArgs
  | CloneRepos CloneReposArgs
  | CreateBranch CreateBranchArgs
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
                (progDesc "Show all repos in an organization that maches a regex")
          )
    <> command
          "clone-repos"
          (info cloneReposCommand
                (progDesc "Clone all repos in an organization that matches a regex")
          )
    <> command
          "create-branch"
          (info createBranchCommand
                (progDesc "Create a new branch for all repos in an organization that matches a regex")
          )
    )

showRepoCommand :: Parser Command
showRepoCommand = ShowRepos <$> showRepoArgsParser

initCommand :: Parser Command
initCommand = Init <$> initArgsParser

cloneReposCommand :: Parser Command
cloneReposCommand = CloneRepos <$> cloneReposArgsParser

createBranchCommand :: Parser Command
createBranchCommand = CreateBranch <$> createBranchArgsParser

