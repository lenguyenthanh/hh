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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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
    )
  where

import Control.Error
import qualified Data.Morpheus.Types as M
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
