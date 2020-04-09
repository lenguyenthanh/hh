{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HH.Cli.Command.InitConfig
    ( InitArgs
    , initArgsParser
    , runSaveConfig
    )
  where

import Control.Monad.Reader
import Data.Bifunctor (bimap)
import Data.Text (Text, pack)
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.FileSystem
import HH.Effect.Github
import HH.Env
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
  :: (MonadReader Env m, MonadConfig m, MonadConsole m, MonadGithub m, MonadFileSystem m)
  => InitArgs -> m ()
runSaveConfig InitArgs {..} = do
  env <- ask
  let conf = appConfig env
  result <- createDirectoryIfMissing root
  case result of
    Left exception ->
      printLn $ "Failed to create root directory " <> root <> "\n" <> (pack $ show exception)
    Right _ -> do
      either <- fetchUsername token
      case either of
        Left err ->
          printLn $ "Failed to verify token: " <> token <> " because of: " <> err
        Right name ->
          saveConfig conf UserConfig { absRootPath = root
                                    , githubToken = token
                                    , githubUsername = name
                                    }
