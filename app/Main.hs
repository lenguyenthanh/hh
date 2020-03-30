{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens
import Command
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import qualified Github.Api as GH
import Options.Applicative
import UserConfig

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
runCommand (ShowRepo { org }) = showRepos org

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("hh version " <> hhVersion)
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)

showRepos :: Text -> IO ()
showRepos org = do
  config <- getConfig
  response <- GH.fetchOrgRepos org (encodeUtf8 $ config^.githubToken)
  case response of
    Left err -> print .unpack $ "Error " <> err
    Right repos -> mapM_ showRepo repos

showRepo :: GH.RemoteRepo -> IO ()
showRepo repo = print $
  "Repo name: " <> repo^.GH.name <> ", url: " <> repo^.GH.url
