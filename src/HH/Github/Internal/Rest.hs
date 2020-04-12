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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}


module HH.Github.Internal.Rest
    ( CreateTeam(..)
    , CreateTeamResponse(..)
    , createTeam
    )
  where

import Data.Aeson
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import HH.Internal.Prelude
import Network.HTTP.Req

data CreateTeam
  = CreateTeam
    { org :: Text
    , name :: Text
    , description :: Maybe Text
    , users :: [Text]
    , privacy :: Text
    , token :: Text
    }
  deriving (Show, Generic)

createTeam :: CreateTeam -> IO CreateTeamResponse
createTeam CreateTeam {..} = runReq defaultHttpConfig $ do
    let hs = headers token
    let body = CreateTeamBody { name = name
                              , description = description
                              , maintainers = users
                              , privacy = privacy
                              }
    responseBody
      <$> req POST
              (https "api.github.com" /: "orgs" /: org /: "teams")
              (ReqBodyJson body)
              jsonResponse
              hs

data CreateTeamBody
  = CreateTeamBody
    { name :: Text
    , description :: Maybe Text
    , maintainers :: [Text]
    , privacy :: Text
    }
  deriving (Generic, Show)

instance ToJSON CreateTeamBody
instance FromJSON CreateTeamBody

data CreateTeamResponse = CreateTeamResponse
    { id :: Int
    , htmlUrl :: String
    }
  deriving (Generic, Show)

instance ToJSON CreateTeamResponse
instance FromJSON CreateTeamResponse where
  parseJSON = genericParseJSON
      defaultOptions { fieldLabelModifier = camelTo2 '_' }

headers token = header "Content-Type" "application/vnd.github.v3+json"
              <> header "User-Agent" "hh"
              <> oAuth2Bearer (encodeUtf8 token)
