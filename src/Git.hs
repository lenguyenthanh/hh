module Git
    (clone
    )
  where

import Shellmet ()
import Data.Text

clone :: Text -> Text -> IO ()
clone url path = "git" ["clone", url, path]
