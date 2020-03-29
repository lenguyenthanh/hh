{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Command
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Github.Api (OrgRepos, fetchRepositories)
import Options.Applicative
import UserConfig (UserConfig(..), getConfig, saveConfig, showConfig)

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
runCommand (Init {..}) = saveConfig root token
runCommand ShowConfig = showConfig
runCommand (ShowRepo { org }) = showRepo org

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("hh version " <> hhVersion)
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)

showRepo :: Text -> IO ()
showRepo org = do
  config <- getConfig
  response <- fetchRepositories org (encodeUtf8 $ githubToken config)
  case response of
    Left err -> print .unpack $ "Error " <> err
    Right repos -> mapM_ (print.show) repos
