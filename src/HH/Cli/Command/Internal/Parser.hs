module HH.Cli.Command.Internal.Parser
    ( organizationParser
    , regexParser
    , useHttpsParser
    , newBranchParser
    , baseBranchParser
    )
  where

import Data.Text
import HH.Internal.Prelude
import Options.Applicative

organizationParser :: Parser Text
organizationParser = strOption
    (long "org" <> short 'o' <> metavar "name" <> help "Organization name")

regexParser :: Parser Text
regexParser = strOption
    (long "regex" <> short 'r' <> metavar "regular expression" <> help "Regular expression to filter repositories")

useHttpsParser :: Parser Bool
useHttpsParser = switch
    (long "use-https" <> short 'u' <>  help "Clone by https, default is ssh")

newBranchParser :: Parser Text
newBranchParser = strOption
    (long "new-branch" <> short 'n' <> metavar "branch" <> help "New branch name")

baseBranchParser :: Parser Text
baseBranchParser = strOption
    (long "base-branch"
    <> short 'b'
    <> value "master"
    <> showDefault
    <> metavar "branch"
    <> help "The branch which you want to base off")
