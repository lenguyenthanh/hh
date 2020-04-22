{-# LANGUAGE FlexibleContexts #-}

module HH.Cli.Command.InitConfig
  ( InitArgs,
    initArgsParser,
    runSaveConfig,
  )
where

import Control.Error
import Control.Exception.Safe (IOException)
import Control.Monad.Reader
import HH.AppConfig
import HH.Effect.Config
import HH.Effect.Console
import HH.Effect.FileSystem
import HH.Effect.Github
import HH.Env
import HH.Internal.Prelude
import Options.Applicative
import Path.Extended

data InitArgs
  = InitArgs
      { root :: Path Abs Dir,
        token :: Text
      }
  deriving (Show)

initArgsParser :: Parser InitArgs
initArgsParser =
  InitArgs
    <$> rootOption
    <*> tokenOption

rootOption :: Parser (Path Abs Dir)
rootOption =
  option
    parseRoot
    (long "root" <> short 'r' <> metavar "<dir>" <> help "Root directory for all repositories. This must be an absolute path.")

tokenOption :: Parser Text
tokenOption =
  strOption
    (long "token" <> short 't' <> metavar "Github token" <> help "We need your github token to query github api")

parseRoot :: ReadM (Path Abs Dir)
parseRoot = eitherReader (first show . parseAbsDir)

runSaveConfig ::
  (MonadReader Env m, MonadConfig m, MonadConsole m, MonadGithub m, MonadFileSystem m) =>
  InitArgs ->
  m ()
runSaveConfig args = do
  env <- ask
  let conf = appConfig env
  result <- runExceptT $ verifyAndSave conf args
  case result of
    Left e -> printLn $ showError e
    Right _ -> printLn "Saved configuration successfully"

verifyAndSave ::
  (MonadConfig m, MonadGithub m, MonadFileSystem m) =>
  AppConfig ->
  InitArgs ->
  ExceptT SaveConfigError m ()
verifyAndSave conf InitArgs {root, token} = do
  fmapLT (CreateDirectoryError root) $ createDirectoryIfMissing root
  name <- fmapLT (VerifyTokenError token) $ fetchUsername token
  fmapLT SaveConfigError . saveConfig conf $
    UserConfig
      { absRootPath = fromAbsDir root,
        githubToken = token,
        githubUsername = name
      }

data SaveConfigError
  = CreateDirectoryError (Path Abs Dir) IOException
  | SaveConfigError IOException
  | VerifyTokenError Text GQLError

showError :: SaveConfigError -> Text
showError (CreateDirectoryError root e) = "Failed to create root directory " <> fromAbsDir root <> "\n" <> pack (show e)
showError (SaveConfigError e) = "Failed to save your configuration" <> "\n" <> pack (show e)
showError (VerifyTokenError token msg) = "Failed to verify token: " <> token <> " because of: " <> (pack . show $ msg)
