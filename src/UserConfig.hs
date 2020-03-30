{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module UserConfig
    (UserConfig(..)
    , absRootPath
    , githubToken
    , githubUsername
    , saveConfig
    , getConfig
    )
  where

import Control.Exception.Safe
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text
import GHC.Generics
import qualified System.Directory as D

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

saveConfig :: FilePath -> UserConfig -> IO ()
saveConfig fPath = (LB.writeFile fPath) . encode

getConfig :: FilePath -> IO UserConfig
getConfig fPath = do
  exist <- D.doesFileExist fPath
  if exist
     then do
        content <- LB.readFile fPath
        case eitherDecode content of
          Left err -> throwString $ "Failed to decode config " <> err
          Right config -> pure config
     else throwString "Config files does not exist. You may need to init it first."

