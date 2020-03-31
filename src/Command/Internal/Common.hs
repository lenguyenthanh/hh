module Command.Internal.Common
    (filterRepos
    , fetchAndFilterRepos
    , concatPath
    )
  where

import Data.Text.Encoding (encodeUtf8)
import Control.Lens
import Data.Text (Text, pack, unpack)
import Github.Api
import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import UserConfig
import System.FilePath ((</>))

filterRepos :: Maybe Text -> [RemoteRepo] -> [RemoteRepo]
filterRepos Nothing x = x
filterRepos (Just regex) xs = filter (\x -> (x^.name) =~ regex :: Bool) xs

fetchAndFilterRepos :: Text -> Maybe Text -> IO (Either Text [RemoteRepo])
fetchAndFilterRepos org regex = do
  config <- getConfig
  response <- fetchOrgRepos org (encodeUtf8 $ config^.githubToken)
  pure $ filterRepos regex <$> response

concatPath :: Foldable t => t Text -> Text
concatPath = pack.(foldr (\x y -> unpack x </> y) "")
