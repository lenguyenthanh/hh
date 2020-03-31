{-# LANGUAGE RecordWildCards #-}

module Command.InitConfig
    ( InitArgs
    , initArgsParser
    , saveConfig
    , showConfig
    )
  where

import Control.Lens
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Github.Api (fetchUsername)
import Options.Applicative
import qualified Path as P
import qualified System.Directory as D
import qualified UserConfig as U
import Util

data InitArgs =
  InitArgs { root :: Text
           , token :: Text
           }
  deriving (Show)

initArgsParser :: Parser InitArgs
initArgsParser = InitArgs
          <$> rootOption
          <*> tokenOption

rootOption :: Parser Text
rootOption = option parseRoot
    (long "root" <> short 'r' <> metavar "<dir>" <> help "Root directory for all repositories")

tokenOption :: Parser Text
tokenOption = strOption
    (long "token" <> short 't' <> metavar "Github token" <> help "We need your github token to query github api")

parseRoot :: ReadM Text
parseRoot = pack <$> eitherReader parseAbsDir

parseAbsDir :: FilePath -> Either String String
parseAbsDir f = bimap show P.fromAbsDir $ P.parseAbsDir f

-- Process command

saveConfig :: InitArgs -> IO ()
saveConfig InitArgs {..} = do
  D.createDirectoryIfMissing True $ unpack root
  either <- validateToken token
  case either of
    Left(err) -> throwText $ "Failed to verify token: " <> token <> " because of: " <> err
    Right(name) -> U.saveConfig $ U.UserConfig { _absRootPath = root
                                               , _githubToken = token
                                               , _githubUsername = name
                                               }

showConfig :: IO ()
showConfig = U.getConfig >>= print.show

validateToken :: Text -> IO (Either Text Text)
validateToken = fetchUsername . encodeUtf8
