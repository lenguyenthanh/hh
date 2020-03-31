{-# LANGUAGE TemplateHaskell #-}

module Github.Api
    (RemoteRepo(..)
    , name
    , sshUrl
    , nameWithOwner
    , url
    , httpsUrl
    , fetchOrgRepos
    , GQL.fetchUsername
    , Rest.createTeam
    )
  where

import qualified Github.Internal.Rest as Rest
import Control.Error.Safe (justErr)
import Control.Lens
import qualified Data.ByteString as BS
import Data.Text
import qualified Github.Internal.GraphQl as GQL
import qualified Data.Morpheus.Types as M


data RemoteRepo =
  RemoteRepo { _name :: Text
             , _sshUrl :: Text
             , _nameWithOwner :: Text
             , _url :: Text
             }
  deriving (Show)

makeLenses ''RemoteRepo

fetchOrgRepos :: Text -> BS.ByteString -> IO (Either Text [RemoteRepo])
fetchOrgRepos org token = do
  response <- GQL.fetchRepositories org token
  pure $ response >>= (justErr "Invalid Response") . sequence . (fmap toRemoteRepo)

toRemoteRepo :: GQL.Repository -> Maybe RemoteRepo
toRemoteRepo repo = RemoteRepo
                  <$> (Just $ GQL.name repo)
                  <*> (scalarToText $ GQL.sshUrl repo)
                  <*> (Just $ GQL.nameWithOwner repo)
                  <*> (scalarToText $ GQL.url repo)

scalarToText :: M.ScalarValue -> Maybe Text
scalarToText (M.String t) = Just t
scalarToText x = Nothing

httpsUrl :: Lens' RemoteRepo Text
httpsUrl = lens getter setter
  where
    getter = (<> ".git") <$> _url
    setter repo text =
      repo {_url = text }
