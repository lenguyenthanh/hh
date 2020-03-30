module Command.Internal.Common
    (filterRepos
    )
  where

import Control.Lens
import Data.Text (Text)
import Github.Api
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

filterRepos :: (Maybe Text) -> [RemoteRepo] -> [RemoteRepo]
filterRepos Nothing x = x
filterRepos (Just regex) xs = filter (\x -> (x^.name) =~ regex :: Bool) xs

