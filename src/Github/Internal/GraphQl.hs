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

import Control.Error.Util (hush)
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

fetchRepositories :: Text -> BS.ByteString -> IO (Either Text [Repository])
fetchRepositories login token = fetchRepositories' login token Nothing

fetchRepositories' :: Text -> BS.ByteString -> Maybe Text -> IO (Either Text [Repository])
fetchRepositories' login token after = do
  response <- fetch (resolver token) args
  let res = first pack response
  let pageInfo = hush res >>= getPageInfo
  let list = res >>= repos
  case pageInfo of
    Just info ->
      if hasNextPage info
         then do
            rec <- fetchRepositories' login token (endCursor info)
            pure $ mappend <$> list <*> rec
         else pure list
    Nothing -> pure list
  where
    args = OrgReposArgs { orgReposArgsLogin = unpack login
                        , orgReposArgsCount = 10
                        , orgReposArgsAfter = fmap unpack after
                        }

repos :: OrgRepos -> Either Text [Repository]
repos org = justErr "Invalid Response" repos
  where
    repos = organization org >>= nodes.repositories
                             >>= sequence

getPageInfo :: OrgRepos -> Maybe OrganizationRepositoriesPageInfoPageInfo
getPageInfo org = fmap (pageInfo.repositories) $ organization org

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
