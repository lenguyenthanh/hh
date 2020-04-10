{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module HH.UserConfig
    (UserConfig(..)
    , saveConfig
    , getConfig
    , GetConfigError(..)
    )
  where

import Control.Exception.Safe (IOException)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text
import GHC.Generics
import HH.AppConfig
import qualified System.Directory as D
import System.FilePath ((</>))
import Control.Monad.Except
import Control.Error

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
  sConfig fPath config
    where
      sConfig :: FilePath -> UserConfig -> ExceptT IOException IO ()
      sConfig fPath = tryIO . LB.writeFile fPath . encode

getConfig :: AppConfig -> ExceptT GetConfigError IO UserConfig
getConfig AppConfig{..} =  do
  fPath <- fmapLT IOEx $ userConfigPath configDir configName
  getConfig' fPath
  where
    getConfig' :: FilePath -> ExceptT GetConfigError IO UserConfig
    getConfig' fPath = do
      exist <- fmapLT IOEx (tryIO $ D.doesFileExist fPath)
      if exist
        then do
            content <- fmapLT IOEx (tryIO $ LB.readFile fPath)
            case eitherDecode content of
              Left err ->
                throwE $ DecodeError err
              Right config ->
                pure config
        else throwE ConfigFileNotExist

data GetConfigError
  = ConfigFileNotExist
  | DecodeError String
  | IOEx IOException
  deriving (Show, Eq)

userConfigPath :: Text -> Text -> ExceptT IOException IO FilePath
userConfigPath dir name = do
      path <- tryIO . D.getXdgDirectory D.XdgConfig . unpack $ dir
      tryIO $ D.createDirectoryIfMissing True path
      pure $ path </> unpack name
