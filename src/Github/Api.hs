{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Github.Api
    (Login(..)
    , fetchUsername
    )
  where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString
import Data.Morpheus.Client (Fetch(..), defineByDocumentFile, gql)
import Data.Text (Text)
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

resolver :: ByteString -> BS.ByteString -> IO BS.ByteString
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

fetchUsername :: ByteString -> IO (Either String Text)
fetchUsername token = (fmap .fmap) username $ fetchLogin token

fetchLogin :: ByteString -> IO (Either String Login)
fetchLogin token = fetch (resolver token) ()

username :: Login -> Text
username = login . viewer
