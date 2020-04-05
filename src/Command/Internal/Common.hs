module Command.Internal.Common
    (filterRepos
    , fetchAndFilterRepos
    , concatPath
    )
  where

import Data.Text (Text, pack, unpack)
import Effect.Config
import Effect.Console
import Effect.Github
import System.FilePath ((</>))
import Text.Regex.TDFA

filterRepos :: Maybe Text -> [RemoteRepo] -> [RemoteRepo]
filterRepos Nothing x = x
filterRepos (Just regex) xs = filter (\x -> (name x) =~ regex :: Bool) xs

fetchAndFilterRepos
  :: (MonadConfig m, MonadConsole m, MonadGithub m)
  => Text -> Text -> Text -> Maybe Text -> m (Either Text [RemoteRepo])
fetchAndFilterRepos configDir configName org regex = do
  conf <- getConfig configDir configName
  let token = githubToken conf
  response <- fetchOrgRepos org token
  pure $ filterRepos regex <$> response

concatPath :: Foldable t => t Text -> Text
concatPath = pack.(foldr (\x y -> unpack x </> y) "")
