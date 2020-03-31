{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Command
import Options.Applicative
import Command.ShowRepos (showRepos)
import Command.InitConfig (saveConfig, showConfig)
import Command.CloneRepos (cloneRepos)
import Command.Create (doCreate)

hhProgDes = "Git multirepo maintenance tool"
hhHeader = "HH - Git from distance"
hhVersion = "0.1.0"

main :: IO ()
main = runCommand =<< execParser opts
  where
    opts = info (commands <**> versionOption <**> helper)
         ( fullDesc
        <> progDesc hhProgDes
        <> header hhHeader)

runCommand :: Command -> IO ()
runCommand (Init args) = saveConfig args
runCommand ShowConfig = showConfig
runCommand (ShowRepos args) = showRepos args
runCommand (CloneRepos args) = cloneRepos args
runCommand (Create args) = doCreate args

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("hh version " <> hhVersion)
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)
