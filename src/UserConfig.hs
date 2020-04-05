{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module UserConfig
    (UserConfig(..)
    , saveConfig
    , getConfig
    )
  where

import Control.Exception.Safe
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text
import GHC.Generics
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

saveConfig :: UserConfig -> IO ()
saveConfig config = userConfigPath >>= (saveConfig' config)
  where
    saveConfig' :: UserConfig -> FilePath -> IO ()
    saveConfig' conf fPath = (LB.writeFile fPath) $ encode conf


getConfig :: IO UserConfig
getConfig = userConfigPath >>= getConfig'
  where
    getConfig' :: FilePath -> IO UserConfig
    getConfig' fPath = do
      exist <- D.doesFileExist fPath
      if exist
        then do
            content <- LB.readFile fPath
            case eitherDecode content of
              Left err -> throwString $ "Failed to decode config " <> err
              Right config -> pure config
        else throwString "Config files does not exist. You may need to init it first."


config_dir = "hh"
config_file = "config.json"

userConfigPath :: IO FilePath
userConfigPath = userConfigPath' config_dir config_file
  where
    userConfigPath' :: FilePath -> FilePath -> IO FilePath
    userConfigPath' path name = do
      dir <- D.getXdgDirectory D.XdgConfig path
      D.createDirectoryIfMissing True dir
      pure $ dir </> name
