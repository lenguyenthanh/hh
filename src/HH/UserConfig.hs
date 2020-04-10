{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module HH.UserConfig
    (UserConfig(..)
    , saveConfig
    , getConfig
    , GetConfigError(..)
    )
  where

import Control.Error
import Control.Exception.Safe (IOException)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text
import GHC.Generics
import HH.AppConfig
import qualified System.Directory as D
import System.FilePath ((</>))

data UserConfig
  = UserConfig
    { absRootPath :: Text
    , githubToken :: Text
    , githubUsername :: Text
    }
  deriving (Generic, Show)

instance ToJSON UserConfig
instance FromJSON UserConfig

saveConfig :: AppConfig -> UserConfig -> ExceptT IOException IO ()
saveConfig (AppConfig{..}) config = do
  fPath <- userConfigPath configDir configName
  saveConfWithPath fPath config

saveConfWithPath :: FilePath -> UserConfig -> ExceptT IOException IO ()
saveConfWithPath fPath = tryIO . LB.writeFile fPath . encode

getConfig :: AppConfig -> ExceptT GetConfigError IO UserConfig
getConfig AppConfig{..} =  do
  fPath <- fmapLT IOError $ userConfigPath configDir configName
  getConfigByPath fPath

getConfigByPath :: FilePath -> ExceptT GetConfigError IO UserConfig
getConfigByPath fPath = do
  isConfFileExist <- fmapLT IOError . tryIO . D.doesFileExist $ fPath
  if isConfFileExist
    then do
        content <- fmapLT IOError . tryIO . LB.readFile $ fPath
        case eitherDecode content of
          Left err ->
            throwE $ DecodeError err
          Right config ->
            pure config
    else throwE FileNotExist

data GetConfigError
  = FileNotExist
  | DecodeError String
  | IOError IOException
  deriving (Show, Eq)

userConfigPath :: Text -> Text -> ExceptT IOException IO FilePath
userConfigPath dir name = do
      path <- tryIO . D.getXdgDirectory D.XdgConfig . unpack $ dir
      tryIO $ D.createDirectoryIfMissing True path
      pure $ path </> unpack name
