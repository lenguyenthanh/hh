module HH.Env
  ( Env (..),
  )
where

import HH.AppConfig
import HH.Internal.Prelude

data Env
  = Env
      { appConfig :: AppConfig
      }
  deriving (Show)
