{-# LANGUAGE RecordWildCards #-}

module Command.CloneRepos
    ( CloneReposArgs(..)
    , cloneReposArgsParser
    , cloneRepos
    )
  where

import Command.Internal.Common
import Command.Internal.Parser
import Control.Lens
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Git (clone)
import Github.Api
import Options.Applicative
import Prelude hiding (putStrLn)
import UserConfig

data CloneReposArgs =
  CloneReposArgs { org :: Text
                 , regex :: Maybe Text
                 , useHttps :: Bool
                 } deriving (Show)

cloneReposArgsParser :: Parser CloneReposArgs
cloneReposArgsParser = CloneReposArgs
               <$> organizationParser
               <*> optional regexParser
               <*> useHttpsParser

cloneRepos :: CloneReposArgs -> IO ()
cloneRepos (CloneReposArgs {..}) = do
  response <- fetchAndFilterRepos org regex
  case response of
    Right repos -> mapM_ (cloneRepo useHttps) repos
    Left err -> putStrLn $ "Error " <> err

cloneRepo :: Bool -> RemoteRepo -> IO ()
cloneRepo useHttps repo = do
  conf <- getConfig
  let path = concatPath [conf^.absRootPath, repo^.nameWithOwner]
  _ <- clone url path
  putStrLn $ "Cloned " <> repo^.name <> " success"
  where
    url = if useHttps
            then repo^.httpsUrl
            else repo^.sshUrl
