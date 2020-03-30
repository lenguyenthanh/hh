{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Command
import Options.Applicative
import Command.ShowRepo (showRepos)
import Command.InitConfig (saveConfig, showConfig)

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
runCommand (ShowRepo args) = showRepos args

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("hh version " <> hhVersion)
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)
