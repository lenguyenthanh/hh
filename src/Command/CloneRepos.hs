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
import Data.Text.IO (putStr)
import Options.Applicative
import Prelude hiding (putStr)
import Github.Api
import Git (clone)
import UserConfig

data CloneReposArgs =
  CloneReposArgs { org :: Text
                 , regex :: Maybe Text
                 , useHttps :: Bool
                 } deriving (Show)

cloneReposArgsParser :: Parser CloneReposArgs
cloneReposArgsParser = CloneReposArgs
               <$> organizationOption
               <*> optional regexOption
               <*> useHttpsParser

useHttpsParser :: Parser Bool
useHttpsParser = switch
    (long "use-https" <> short 'u' <>  help "Clone by https, default is ssh")

cloneRepos :: CloneReposArgs -> IO ()
cloneRepos (CloneReposArgs {..}) = do
  response <- fetchAndFilterRepos org regex
  case response of
    Right repos -> mapM_ (cloneRepo useHttps) repos
    Left err -> putStr $ "Error " <> err

cloneRepo :: Bool -> RemoteRepo -> IO ()
cloneRepo useHttps repo = do
  config <- getConfig
  clone url $ config^.absRootPath <> repo^.nameWithOwner
  where
    url = if useHttps
            then repo^.httpsUrl
            else repo^.sshUrl
