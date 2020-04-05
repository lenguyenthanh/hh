{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Github.Internal.Rest
    ( CreateTeam(..)
    , CreateTeamResponse(..)
    , createTeam
    )
  where

import Data.Aeson
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Req

data CreateTeam
  = CreateTeam
    { createTeamOrg :: Text
    , createTeamName :: Text
    , createTeamDescription :: Maybe Text
    , createTeamUsers :: [Text]
    , createTeamPrivacy :: Text
    , createTeamToken :: Text
    }
  deriving (Show)

createTeam :: CreateTeam -> IO CreateTeamResponse
createTeam CreateTeam {..} = runReq defaultHttpConfig $ do
    let hs = headers createTeamToken
    let body = CreateTeamBody { name = createTeamName
                              , description = createTeamDescription
                              , maintainers = createTeamUsers
                              , privacy = createTeamPrivacy
                              }
    responseBody
      <$> req POST
              (https "api.github.com" /: "orgs" /: createTeamOrg /: "teams")
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
