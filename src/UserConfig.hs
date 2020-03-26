{-# LANGUAGE DeriveGeneric #-}

module UserConfig
    (UserConfig(..)
    , userConfig
    , saveConfig
    , showConfig
    )
  where

import System.FilePath ((</>))
import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Text
import GHC.Generics
import Options.Applicative
import qualified Path as P
import System.Directory
import Control.Exception.Safe
import qualified Data.ByteString.Lazy as LB

data UserConfig = UserConfig {
    absRootPath :: String,
    githubToken :: String
} deriving (Generic, Show)

instance ToJSON UserConfig
instance FromJSON UserConfig

-- Parse command line input
userConfig :: Parser UserConfig
userConfig = UserConfig
          <$> option parseRoot
            (long "root"
            <> short 'r'
            <> metavar "dir"
            <> help "Root directory for all repositories")
          <*> strOption
            (long "token"
            <> short 't'
            <> metavar "Github token"
            <> help "We need your github token to query github api")

parseRoot :: ReadM String
parseRoot = eitherReader parseAbsDir

parseAbsDir :: FilePath -> Either String String
parseAbsDir f = bimap show P.fromAbsDir $ P.parseAbsDir f

-- Process command

config_dir = "hh"
config_file = "config.json"

saveConfig :: UserConfig -> IO ()
saveConfig config@(UserConfig root token) = do
  confPath <- userConfigPath config_dir config_file
  print confPath
  createDirectoryIfMissing True root
  saveConfigWithPath confPath config

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
