{-# LANGUAGE RecordWildCards #-}

module Command.ShowRepo
    ( ShowRepArgs(..)
    , showRepoArgsParser
    , showRepos
    )
  where

import Command.Internal.Path
import Command.Internal.Common
import Command.Internal.Parser
import Control.Lens
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (putStr)
import qualified Github.Api as GH
import Options.Applicative
import Prelude hiding (putStr)
import UserConfig

data ShowRepArgs =
  ShowRepArgs { org :: Text
              , regex :: Maybe Text
              }
  deriving (Show)

showRepoArgsParser :: Parser ShowRepArgs
showRepoArgsParser = ShowRepArgs
               <$> organizationOption
               <*> optional regexOption

showRepos :: ShowRepArgs -> IO ()
showRepos (ShowRepArgs {..}) = do
  path <- userConfigPath
  config <- getConfig path
  response <- GH.fetchOrgRepos org (encodeUtf8 $ config^.githubToken)
  case response of
    Right repos -> mapM_ showRepo $ filterRepos regex repos
    Left err -> putStr $ "Error " <> err

showRepo :: GH.RemoteRepo -> IO ()
showRepo repo = print $
  "Repo name: " <> repo^.GH.name <> ", url: " <> repo^.GH.url
