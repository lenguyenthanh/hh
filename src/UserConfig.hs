{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module UserConfig
    (UserConfig(..)
    , absRootPath
    , githubToken
    , githubUsername
    , saveConfig
    , showConfig
    , getConfig
    )
  where

import Control.Exception.Safe
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import qualified System.Directory as D
import System.FilePath ((</>))

import Github.Api

data UserConfig =
  UserConfig
    { _absRootPath :: Text
    , _githubToken :: Text
    , _githubUsername :: Text
    }
  deriving (Generic, Show)

makeLenses ''UserConfig

instance ToJSON UserConfig
instance FromJSON UserConfig

-- Process command

saveConfig :: Text -> Text -> IO ()
saveConfig root token = do
  confPath <- userConfigPath
  print confPath
  D.createDirectoryIfMissing True $ unpack root
  either <- validateToken token
  case either of
    Left(err) -> throwString . unpack $ "Failed to verify token: " <> token <> " because of: " <> err
    Right(name) -> saveConfigWithPath confPath $ UserConfig { _absRootPath = root
                                                            , _githubToken = token
                                                            , _githubUsername = name
                                                            }

showConfig :: IO ()
showConfig = do
  config <- getConfig
  print $ show config

saveConfigWithPath :: FilePath -> UserConfig -> IO ()
saveConfigWithPath fPath = (LB.writeFile fPath) . encode

getConfig :: IO UserConfig
getConfig =  userConfigPath >>= getConfig'

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

validateToken :: Text -> IO (Either Text Text)
validateToken = fetchUsername . encodeUtf8
