{-# LANGUAGE OverloadedStrings #-}

module Git
    (clone
    , newBranch
    , isGitDir
    , pushBranch
    )
  where

import Data.Text
import Shellmet

clone :: Text -> Text -> IO ()
clone url path = "git" ["clone", url, path]

newBranch :: Text -> Text -> Text -> IO ()
newBranch path new base =
  "git" ["-C", path, "branch", "-c", base, new]

isGitDir :: Text -> IO Bool
isGitDir path = run $
  "git" ["-C", path, "rev-parse", "--is-inside-work-tree"]

pushBranch :: Text -> Text -> IO ()
pushBranch path branch =
  "git" ["-C", path, "push", "origin", branch]

run :: IO () -> IO Bool
run io = io >> pure True $? pure False
