module Util
    (throwText
    )
  where

import Control.Exception.Safe
import Data.Text

throwText :: Text -> IO ()
throwText = throwString . unpack
