module HH.Env
    ( Env(..)
    )
  where

import HH.AppConfig

data Env
  = Env
    { appConfig :: AppConfig
    }
  deriving (Show)
