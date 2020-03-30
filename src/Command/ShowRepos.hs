{-# LANGUAGE RecordWildCards #-}

module Command.ShowRepos
    ( ShowRepArgs(..)
    , showRepoArgsParser
    , showRepos
    )
  where

import Command.Internal.Common
import Command.Internal.Parser
import Control.Lens
import Data.Text (Text)
import Data.Text.IO (putStr)
import Options.Applicative
import Prelude hiding (putStr)
import Github.Api

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
  response <- fetchAndFilterRepos org regex
  case response of
    Right repos -> mapM_ showRepo repos
    Left err -> putStr $ "Error " <> err

showRepo :: RemoteRepo -> IO ()
showRepo repo = print $
  "Repo name: " <> repo^.name <> ", url: " <> repo^.url
