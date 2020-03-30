{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Command
import Control.Lens
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Github.Api as GH
import Options.Applicative
import Text.Regex.TDFA
import Text.Regex.TDFA.Text
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
runCommand (ShowRepo {..}) = showRepos org regex

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("hh version " <> hhVersion)
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)

showRepos :: Text -> Maybe Text -> IO ()
showRepos org regex = do
  config <- getConfig
  response <- GH.fetchOrgRepos org (encodeUtf8 $ config^.githubToken)
  case response of
    Right repos -> mapM_ showRepo $ filterRepos regex repos
    Left err -> print .unpack $ "Error " <> err

filterRepos :: (Maybe Text) -> [GH.RemoteRepo] -> [GH.RemoteRepo]
filterRepos Nothing x = x
filterRepos (Just regex) xs = filter (\x -> (x^.GH.name) =~ regex :: Bool) xs

showRepo :: GH.RemoteRepo -> IO ()
showRepo repo = print $
  "Repo name: " <> repo^.GH.name <> ", url: " <> repo^.GH.url
