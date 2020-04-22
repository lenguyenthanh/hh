module Path.Extended
  ( module P1,
    fromAbsDir,
  )
where

import HH.Internal.Prelude
import qualified Path as P
import Path as P1 hiding (fromAbsDir)

fromAbsDir :: P.Path P.Abs P.Dir -> Text
fromAbsDir = pack . P.fromAbsDir
