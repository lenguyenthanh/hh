{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module HH.Github.Api
    (RemoteRepo(..)
    , httpsUrl
    , fetchOrgRepos
    , Rest.CreateTeam(..)
    , Rest.CreateTeamResponse(..)
    , Rest.createTeam
    , GQL.fetchUsername
    , GQL.GQLError(..)
    , pickUrl
    )
  where

import Control.Error
import Data.Morpheus.Extended (scalarToText)
import Data.Text
import GHC.Generics
import qualified HH.Github.Internal.GraphQl as GQL
import qualified HH.Github.Internal.Rest as Rest
import HH.Internal.Prelude

data RemoteRepo
  = RemoteRepo
    { name :: Text
    , sshUrl :: Text
    , nameWithOwner :: Text
    , url :: Text
    }
  deriving (Show, Generic)

pickUrl :: Bool -> RemoteRepo -> Text
pickUrl useHttps repo = if useHttps
                           then httpsUrl repo
                           else url repo

fetchOrgRepos :: Text -> Text -> ExceptT GQL.GQLError IO [RemoteRepo]
fetchOrgRepos org token = do
  response <- GQL.fetchRepositories org token
  case traverse toRemoteRepo response of
    Just list ->
      pure list
    Nothing ->
      ExceptT . pure . Left $ GQL.InvalidResponse

toRemoteRepo :: GQL.Repository -> Maybe RemoteRepo
toRemoteRepo repo = RemoteRepo
                  <$> Just (GQL.name repo)
                  <*> scalarToText (GQL.sshUrl repo)
                  <*> Just (GQL.nameWithOwner repo)
                  <*> scalarToText (GQL.url repo)

httpsUrl :: RemoteRepo -> Text
httpsUrl = (<> ".git") <$> url
