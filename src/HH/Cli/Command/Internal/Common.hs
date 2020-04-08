module HH.Cli.Command.Internal.Common
    (filterRepos
    , fetchAndFilterRepos
    , concatPath
    )
  where

import Data.Text (Text, pack, unpack)
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.Github
import System.FilePath ((</>))
import Text.Regex.TDFA

filterRepos :: Maybe Text -> [RemoteRepo] -> [RemoteRepo]
filterRepos Nothing x = x
filterRepos (Just regex) xs = filter (\x -> (name x) =~ regex :: Bool) xs

fetchAndFilterRepos
  :: (MonadConsole m, MonadGithub m)
  => UserConfig -> Text -> Maybe Text -> m (Either Text [RemoteRepo])
fetchAndFilterRepos conf org regex = do
  let token = githubToken conf
  response <- fetchOrgRepos org token
  pure $ filterRepos regex <$> response

concatPath :: Foldable t => t Text -> Text
concatPath = pack.(foldr (\x y -> unpack x </> y) "")
