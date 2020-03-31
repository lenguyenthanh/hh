{-# LANGUAGE RecordWildCards #-}

module Command.CreateBranch
    ( CreateBranchArgs(..)
    , createBranchArgsParser
    , createBranch
    )
  where

import Control.Monad (guard)
import Command.Internal.Common
import Command.Internal.Parser
import Control.Lens
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import qualified Git as G
import Github.Api
import Options.Applicative
import Prelude hiding (putStrLn)
import UserConfig

data CreateBranchArgs =
  CreateBranchArgs { org :: Text
                   , regex :: Maybe Text
                   , newBranch :: Text
                   , baseBranch :: Text
                   , useHttps :: Bool
                   } deriving (Show)

createBranchArgsParser :: Parser CreateBranchArgs
createBranchArgsParser = CreateBranchArgs
               <$> organizationParser
               <*> optional regexParser
               <*> newBranchParser
               <*> baseBranchParser
               <*> useHttpsParser

createBranch :: CreateBranchArgs -> IO ()
createBranch (CreateBranchArgs {..}) = do
  response <- fetchAndFilterRepos org regex
  case response of
    Right repos -> mapM_ (mkBranch useHttps newBranch baseBranch) repos
    Left err -> putStrLn $ "Error " <> err

mkBranch :: Bool -> Text -> Text -> RemoteRepo -> IO ()
mkBranch useHttps newBranch baseBranch repo = do
  conf <- getConfig
  let path = concatPath [conf^.absRootPath, repo^.nameWithOwner]
  isGitDir <- G.isGitDir path
  b <- doBranch isGitDir path
  guard b >> G.pushBranch path newBranch
  where
    url = if useHttps
            then repo^.httpsUrl
            else repo^.sshUrl
    mkBranch' path = G.newBranch path newBranch baseBranch
    doBranch isGitDir path = if isGitDir
                              then mkBranch' path
                              else G.clone path url >> mkBranch' path
