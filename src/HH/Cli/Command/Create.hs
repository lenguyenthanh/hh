{-# LANGUAGE FlexibleContexts #-}

module HH.Cli.Command.Create
    (CreateArgs(..)
    , createArgsParser
    , runCreate
    )
  where

import Control.Monad.Reader
import HH.Cli.Command.CreateBranch
import HH.Cli.Command.CreateTeam
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Git
import HH.Effect.Github
import Options.Applicative

data CreateArgs
  = Team (CreateTeamArgs)
  | Branch (CreateBranchArgs)
  deriving (Show)

createArgsParser :: Parser CreateArgs
createArgsParser = hsubparser
    ( command
        "team"
        (info teamParser
              (progDesc "Create new team for an organization")
        )
    <> command
        "branch"
        (info branchParser
              (progDesc "Create a new branch for all repos in an organization that matches a regex")
        )
    )

runCreate
  :: (MonadConsole m, MonadGithub m, MonadGit m, MonadReader UserConfig m)
  => CreateArgs -> m ()
runCreate (Team args) = runCreateTeam args
runCreate (Branch args) = runCreateBranch args

teamParser :: Parser CreateArgs
teamParser = Team <$> createTeamArgsParser

branchParser :: Parser CreateArgs
branchParser = Branch <$> createBranchArgsParser
