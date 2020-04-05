{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module AppConfig
    ( AppConfig(..)
    , getAppConfig
    )
  where

import Dhall

data AppConfig
  = AppConfig
    { configDir :: Text
    , configName :: Text
    }
  deriving (Generic, Show)

instance FromDhall AppConfig

getAppConfig :: IO AppConfig
getAppConfig = input auto "./config.dhall"
