{-# LANGUAGE RecordWildCards #-}

module Command.InitConfig
    ( InitArgs
    , initArgsParser
    , runShowConfig
    , runSaveConfig
    )
  where

import App
import Data.Bifunctor (bimap)
import Data.Text (Text, pack)
import Effect.Config
import Effect.Console
import Effect.FileSystem
import Effect.Github
import Options.Applicative
import qualified Path as P

data InitArgs
  = InitArgs
    { root :: Text
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

runSaveConfig
  :: (MonadConfig m, MonadConsole m, MonadGithub m, MonadFileSystem m)
  => InitArgs -> AppM m ()
runSaveConfig InitArgs {..} = do
  createDirectoryIfMissing root
  either <- fetchUsername token
  case either of
    Left(err) -> printLn $ "Failed to verify token: " <> token <> " because of: " <> err
    Right(name) -> saveConfig UserConfig { absRootPath = root
                                         , githubToken = token
                                         , githubUsername = name
                                         }

runShowConfig
  :: (MonadConfig m, MonadConsole m)
  => AppM m ()
runShowConfig = do
  conf <- getConfig
  printLn.pack $ show conf
