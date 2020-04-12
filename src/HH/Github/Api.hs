{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module HH.Github.Api
    (RemoteRepo(..)
    , Rest.CreateTeam(..)
    , Rest.CreateTeamResponse(..)
    , httpsUrl
    , fetchOrgRepos
    , GQL.fetchUsername
    , GQL.GQLError(..)
    , Rest.createTeam
    )
  where

import Control.Error
import qualified Data.Morpheus.Types as M
import Data.Text
import qualified HH.Github.Internal.GraphQl as GQL
import qualified HH.Github.Internal.Rest as Rest

data RemoteRepo
  = RemoteRepo
    { name :: Text
    , sshUrl :: Text
    , nameWithOwner :: Text
    , url :: Text
    }
  deriving (Show)

fetchOrgRepos :: Text -> Text -> ExceptT GQL.GQLError IO [RemoteRepo]
fetchOrgRepos org token = do
  response <- GQL.fetchRepositories org token
  case sequence . fmap toRemoteRepo $ response of
    Just list ->
      pure list
    Nothing ->
      ExceptT . pure . Left $ GQL.InvalidResponse

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
