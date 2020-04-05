{-# LANGUAGE TemplateHaskell #-}

module Github.Api
    (RemoteRepo(..)
    , Rest.CreateTeam(..)
    , Rest.CreateTeamResponse(..)
    , httpsUrl
    , fetchOrgRepos
    , GQL.fetchUsername
    , Rest.createTeam
    )
  where

import Control.Error.Safe (justErr)
import qualified Data.Morpheus.Types as M
import Data.Text
import qualified Github.Internal.GraphQl as GQL
import qualified Github.Internal.Rest as Rest

data RemoteRepo
  = RemoteRepo
    { name :: Text
    , sshUrl :: Text
    , nameWithOwner :: Text
    , url :: Text
    }
  deriving (Show)

fetchOrgRepos :: Text -> Text -> IO (Either Text [RemoteRepo])
fetchOrgRepos org token = do
  response <- GQL.fetchRepositories org token
  pure $ response >>= (justErr "Invalid Response") . sequence . (fmap toRemoteRepo)

toRemoteRepo :: GQL.Repository -> Maybe RemoteRepo
toRemoteRepo repo = RemoteRepo
                  <$> (Just $ GQL.name repo)
                  <*> (scalarToText $ GQL.sshUrl repo)
                  <*> (Just $ GQL.nameWithOwner repo)
                  <*> (scalarToText $ GQL.url repo)

scalarToText :: M.ScalarValue -> Maybe Text
scalarToText (M.String t) = Just t
scalarToText x = Nothing

httpsUrl :: RemoteRepo -> Text
httpsUrl = (<> ".git") <$> url
