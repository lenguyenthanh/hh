module Data.Morpheus.Extended
    (module Data.Morpheus.Client
    , module Data.Morpheus.Types
    , scalarToText
    )
  where

import Data.Morpheus.Client
import Data.Morpheus.Types
import Data.Text (Text)
import HH.Internal.Prelude

scalarToText :: ScalarValue -> Maybe Text
scalarToText (String t) = Just t
scalarToText _ = Nothing

