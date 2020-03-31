{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Github.Internal.Rest
    ( createTeam
    )
  where

import qualified Data.ByteString as BS
import Data.Text
import Data.Aeson
import Network.HTTP.Req
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text.Encoding (encodeUtf8)

createTeam :: Text -> Text -> Maybe Text -> [Text] -> Text -> Text -> IO CreateTeamResponse
createTeam org team des users privacy token = runReq defaultHttpConfig $ do
    let hs = header "Content-Type" "application/vnd.github.v3+json"
             <> header "User-Agent" "hh"
             <> oAuth2Bearer (encodeUtf8 token)
    let body = CreateTeamBody { name = team
                              , description = des
                              , maintainers = users
                              , privacy = privacy
                              }
    responseBody
      <$> req POST
              (https "api.github.com" /: "orgs" /: org /: "teams")
              (ReqBodyJson body)
              jsonResponse
              hs

data CreateTeamBody =
  CreateTeamBody { name :: Text
                 , description :: Maybe Text
                 , maintainers :: [Text]
                 , privacy :: Text
                 }
  deriving (Generic, Show)

instance ToJSON CreateTeamBody
instance FromJSON CreateTeamBody

data CreateTeamResponse =
  CreateTeamResponse { id :: Int
                     , htmlUrl :: String
                     }
  deriving (Generic, Show)

instance ToJSON CreateTeamResponse
instance FromJSON CreateTeamResponse where
  parseJSON = genericParseJSON
      defaultOptions { fieldLabelModifier = camelTo2 '_' }
