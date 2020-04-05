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

saveConfig :: Text -> Text -> UserConfig -> IO ()
saveConfig dir name config =
  userConfigPath dir name
    >>= (saveConfig' config)
  where
    saveConfig' :: UserConfig -> FilePath -> IO ()
    saveConfig' conf fPath = (LB.writeFile fPath) $ encode conf


getConfig :: Text -> Text -> IO UserConfig
getConfig dir name = userConfigPath dir name >>= getConfig'
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


userConfigPath :: Text -> Text -> IO FilePath
userConfigPath dir name = do
      path <- D.getXdgDirectory D.XdgConfig $ unpack dir
      D.createDirectoryIfMissing True path
      pure $ path </> unpack name
