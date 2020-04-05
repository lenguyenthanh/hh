{-# LANGUAGE FlexibleContexts #-}

module Command.Create
    (CreateArgs(..)
    , createArgsParser
    , runCreate
    )
  where

import Command.CreateBranch
import Command.CreateTeam
import Control.Monad.Reader
import Effect.Config
import Effect.Console
import Effect.Git
import Effect.Github
import Env
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

runCreate :: (MonadConfig m, MonadConsole m, MonadGithub m, MonadGit m, MonadReader Env m) => CreateArgs -> m ()
runCreate (Team args) = runCreateTeam args
runCreate (Branch args) = runCreateBranch args

teamParser :: Parser CreateArgs
teamParser = Team <$> createTeamArgsParser

branchParser :: Parser CreateArgs
branchParser = Branch <$> createBranchArgsParser
