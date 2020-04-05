{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Github.Internal.GraphQl
    ( fetchUsername
    , fetchRepositories
    , OrganizationRepositoriesNodesRepository(..)
    , Repository
    )
  where

import Control.Error.Safe (justErr)
import Control.Error.Util (hush)
import Control.Monad ((<=<))
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Morpheus.Client (Fetch(..), defineByDocumentFile, gql)
import Data.Morpheus.Types (ScalarValue(..))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
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

fetchRepositories :: Text -> Text -> IO (Either Text [Repository])
fetchRepositories login token = fetchRepositories' login (encodeUtf8 token) Nothing

fetchRepositories' :: Text -> BS.ByteString -> Maybe Text -> IO (Either Text [Repository])
fetchRepositories' login token after = do
  response <- fetch (resolver token) args
  let res = first pack response
  let pageInfo = hush res >>= getPageInfo
  let list = res >>= getRepos
  case pageInfo of
    Just info ->
      if hasNextPage info
         then do
            rec <- fetchRepositories' login token (endCursor info)
            pure $ mappend <$> list <*> rec
         else pure list
    Nothing ->
      pure list
  where
    args = OrgReposArgs { orgReposArgsLogin = unpack login
                        , orgReposArgsCount = 10
                        , orgReposArgsAfter = fmap unpack after
                        }

getRepos :: OrgRepos -> Either Text [Repository]
getRepos = justErr "Invalid Response" .
  (sequenceA <=< nodes.repositories <=< organization)

getPageInfo :: OrgRepos -> Maybe OrganizationRepositoriesPageInfoPageInfo
getPageInfo = pure.pageInfo.repositories <=< organization

fetchUsername :: Text -> IO (Either Text Text)
fetchUsername token = do
  login <- fetchLogin $ encodeUtf8 token
  pure $ bimap pack username login


fetchLogin :: BS.ByteString -> IO (Either String Login)
fetchLogin token = fetch (resolver token) ()

username :: Login -> Text
username = login . viewer

resolver :: BS.ByteString -> BL.ByteString -> IO BL.ByteString
resolver token b = runReq defaultHttpConfig $ do
    let headers = header "Content-Type" "application/json"
                <> header "User-Agent" "hh"
                <> oAuth2Bearer token
    responseBody
        <$> req POST
                (https "api.github.com" /: "graphql")
                (ReqBodyLbs b)
                lbsResponse
                headers
