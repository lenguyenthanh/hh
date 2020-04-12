{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HH.Github.Internal.GraphQl
    ( fetchUsername
    , fetchRepositories
    , OrganizationRepositoriesNodesRepository(..)
    , Repository
    , GQLError(..)
    )
  where

import Control.Error
import Control.Exception.Safe
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Morpheus.Client (Fetch(..), defineByDocumentFile, gql)
import Data.Morpheus.Types (ScalarValue(..))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import HH.Internal.Prelude
import Network.HTTP.Req

defineByDocumentFile
  "assets/github.graphql"
  [gql|
    query Login {
      viewer {
        login
      }
    }
  |]

defineByDocumentFile
  "assets/github.graphql"
  [gql|
    query OrgRepos ($login: String!, $count: Int!, $after: String)
    {
      organization(login: $login) {
        repositories(first: $count, after: $after) {
          totalCount
          nodes{
            name,
            nameWithOwner,
            sshUrl,
            url
          }
          pageInfo {
            endCursor
            hasNextPage
          }
        }
      }
    }
  |]

type Repository = OrganizationRepositoriesNodesRepository

data GQLError
    = GQLError Text
    | HttpError HttpException
    | InvalidResponse
  deriving (Show)

fetchRepositories :: Text -> Text -> ExceptT GQLError IO [Repository]
fetchRepositories login token = fetchRepositories' login (encodeUtf8 token) Nothing

fetchRepositories' :: Text -> BS.ByteString -> Maybe Text -> ExceptT GQLError IO [Repository]
fetchRepositories' login token after = do
  response <- safeIO $ fetch (resolver token) args
  let pageInfo = getPageInfo response
  case getRepos response of
    Left e ->
      ExceptT . pure . Left $ e
    Right list ->
      case pageInfo of
        Just info ->
          if hasNextPage info
            then do
                rec <- fetchRepositories' login token (endCursor info)
                pure $ list <> rec
            else
                pure list
        Nothing ->
          pure list
  where
    args = OrgReposArgs { orgReposArgsLogin = unpack login
                        , orgReposArgsCount = 10
                        , orgReposArgsAfter = fmap unpack after
                        }

getRepos :: OrgRepos -> Either GQLError [Repository]
getRepos = justErr InvalidResponse .
  (sequenceA <=< nodes.repositories <=< organization)

getPageInfo :: OrgRepos -> Maybe OrganizationRepositoriesPageInfoPageInfo
getPageInfo = pure.pageInfo.repositories <=< organization

fetchUsername :: Text -> ExceptT GQLError IO Text
fetchUsername = fmap username . fetchLogin. encodeUtf8

fetchLogin :: BS.ByteString -> ExceptT GQLError IO Login
fetchLogin token = safeIO $ fetch (resolver token) ()

username :: Login -> Text
username = login . viewer

resolver :: BS.ByteString -> BL.ByteString -> IO BL.ByteString
resolver token body = runReq defaultHttpConfig $ do
    let headers = header "Content-Type" "application/json"
                <> header "User-Agent" "hh"
                <> oAuth2Bearer token
    responseBody
        <$> req POST
                (https "api.github.com" /: "graphql")
                (ReqBodyLbs body)
                lbsResponse
                headers

safeIO :: forall a. IO (Either String a) -> ExceptT GQLError IO a
safeIO io = ExceptT $
  try io >>= \either ->
    case either of
      Left (e :: HttpException) ->
        pure . Left . HttpError $ e
      Right r ->
        pure . fmapL (GQLError . pack) $ r
