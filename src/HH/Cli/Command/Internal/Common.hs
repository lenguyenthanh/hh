module HH.Cli.Command.Internal.Common
    (filterRepos
    , fetchAndFilterRepos
    )
  where

import Control.Monad.Except
import Data.Text (Text)
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Github
import Text.Regex.TDFA

filterRepos :: Maybe Text -> [RemoteRepo] -> [RemoteRepo]
filterRepos Nothing x = x
filterRepos (Just regex) xs = filter (\x -> (name x) =~ regex :: Bool) xs

fetchAndFilterRepos
  :: (MonadConsole m, MonadGithub m)
  => UserConfig -> Text -> Maybe Text -> ExceptT GQLError m [RemoteRepo]
fetchAndFilterRepos conf org regex = do
  let token = githubToken conf
  response <- fetchOrgRepos org token
  pure $ filterRepos regex response
