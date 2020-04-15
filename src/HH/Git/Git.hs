{-# LANGUAGE OverloadedStrings #-}

module HH.Git.Git
  ( clone,
    newBranch,
    isGitDir,
    pushBranch,
  )
where

import Data.Text
import HH.Internal.Prelude
import qualified Turtle as T

clone :: Text -> Text -> IO Bool
clone url path = run "git" ["clone", url, path]

newBranch :: Text -> Text -> Text -> IO Bool
newBranch path new base = run "git" ["-C", path, "branch", "-c", base, new]

isGitDir :: Text -> IO Bool
isGitDir path = run "git" ["-C", path, "rev-parse", "--is-inside-work-tree"]

pushBranch :: Text -> Text -> IO Bool
pushBranch path branch = run "git" ["-C", path, "push", "origin", branch]

isSuccess :: T.ExitCode -> Bool
isSuccess T.ExitSuccess = True
isSuccess (T.ExitFailure _) = False

run :: T.MonadIO io => Text -> [Text] -> io Bool
run cmd args = isSuccess <$> T.proc cmd args T.empty
