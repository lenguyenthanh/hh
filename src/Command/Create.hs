
module Command.Create
    (CreateArgs(..)
    , createArgsParser
    , doCreate
    )
  where

import Command.CreateBranch
import Command.CreateTeam
import Options.Applicative

data CreateArgs =
    Team(CreateTeamArgs)
  | Branch(CreateBranchArgs)
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

doCreate :: CreateArgs -> IO ()
doCreate (Team args) = createTeam args
doCreate (Branch args) = createBranch args

teamParser :: Parser CreateArgs
teamParser = Team <$> createTeamArgsParser

branchParser :: Parser CreateArgs
branchParser = Branch <$> createBranchArgsParser
