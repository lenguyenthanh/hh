module System.FilePath.Extended
  ( module System.FilePath,
    concatPath,
  )
where

import HH.Internal.Prelude
import System.FilePath

concatPath :: Foldable t => t Text -> Text
concatPath = pack . foldr (\x y -> unpack x </> y) ""
