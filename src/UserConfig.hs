{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module UserConfig
    (UserConfig(..)
    , saveConfig
    , showConfig
    )
  where

import System.FilePath ((</>))
import Data.Aeson
import Data.Text
import GHC.Generics
import System.Directory
import Control.Exception.Safe
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU

import Github.Api

data UserConfig = UserConfig {
    absRootPath :: String,
    githubToken :: String,
    githubUsername :: String
} deriving (Generic, Show)

instance ToJSON UserConfig
instance FromJSON UserConfig

-- Process command

config_dir = "hh"
config_file = "config.json"

saveConfig :: String -> String -> IO ()
saveConfig root token = do
  confPath <- userConfigPath config_dir config_file
  print confPath
  createDirectoryIfMissing True root
  either <- validateToken token
  case either of
    Left(err) -> throwString $ "Failed to verify token: " <> token <> " because of: " <> err
    Right(name) -> saveConfigWithPath confPath $ UserConfig {absRootPath = root
                                                            , githubToken = token
                                                            , githubUsername = name}

showConfig :: IO ()
showConfig = do
  fPath <- userConfigPath config_dir config_file
  config <- getConfig fPath
  print $ show config

saveConfigWithPath :: FilePath -> UserConfig -> IO ()
saveConfigWithPath fPath = (LB.writeFile fPath) . encode

getConfig :: FilePath -> IO UserConfig
getConfig fPath = do
  exist <- doesFileExist fPath
  if exist
     then do
        content <- LB.readFile fPath
        case eitherDecode content of
          Left err -> throwString $ "Failed to decode config " <> err
          Right config -> pure config
     else throwString "Config files does not exist. You may need to init it first."

userConfigPath :: FilePath -> FilePath -> IO FilePath
userConfigPath path name = do
  dir <- getXdgDirectory XdgConfig path
  createDirectoryIfMissing True dir
  pure $ dir </> name

configDir :: String -> IO FilePath
configDir = getXdgDirectory XdgConfig

validateToken :: String -> IO (Either String String)
validateToken token = do
  username <- fetchUsername $ BU.fromString token
  print $ show username
  pure $ fmap unpack username
