module HH.Cli.Command
    ( Command(..)
    , commands
    )
  where

import HH.Cli.Command.CloneRepos
import HH.Cli.Command.Create
import HH.Cli.Command.InitConfig
import HH.Cli.Command.ShowRepos
import Options.Applicative

data Command
  = ShowConfig
  | Init InitArgs
  | ShowRepos ShowRepArgs
  | CloneRepos CloneReposArgs
  | Create CreateArgs
  deriving (Show)



commandParser :: Parser Command
commandParser = hsubparser
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
          "create"
          (info createParser
                (progDesc "Create team, branch, discussion")
          )
    )

showRepoCommand :: Parser Command
showRepoCommand = ShowRepos <$> showRepoArgsParser

initCommand :: Parser Command
initCommand = Init <$> initArgsParser

cloneReposCommand :: Parser Command
cloneReposCommand = CloneRepos <$> cloneReposArgsParser

createParser :: Parser Command
createParser = Create <$> createArgsParser


hhProgDes = "Git multirepo maintenance tool"
hhHeader = "HH - Git from distance"
hhVersion = "0.1.0"

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("hh version " <> hhVersion)
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)

commands :: IO Command
commands = execParser $
  info (commandParser <**> versionOption <**> helper)
      ( fullDesc
      <> progDesc hhProgDes
      <> header hhHeader)
