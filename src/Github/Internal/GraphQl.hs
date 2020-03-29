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

import Data.Morpheus.Client (Fetch(..), defineByDocumentFile, gql)
import Data.Morpheus.Types (ScalarValue(..))
import Data.Text (Text, pack, unpack)
import Network.HTTP.Req
import Control.Error.Safe (justErr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Bifunctor (bimap, first)

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
    query OrgRepos ($login: String!, $count: Int!)
    {
      organization(login: $login) {
        repositories(first: $count) {
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

fetchRepositories :: Text -> BS.ByteString -> IO (Either Text [Repository])
fetchRepositories login token = do
  response <- fetch (resolver token) args
  pure $ first pack response >>= repos
  where
    args = OrgReposArgs { orgReposArgsLogin = unpack login
                        , orgReposArgsCount = 10 }

repos :: OrgRepos -> Either Text [OrganizationRepositoriesNodesRepository]
repos org = justErr "Invalid Response" repos
  where
    repos = organization org >>= nodes .repositories
                             >>= sequence
fetchUsername :: BS.ByteString -> IO (Either Text Text)
fetchUsername token = do
  login <- fetchLogin token
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
