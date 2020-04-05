module Env
    ( Env(..)
    )
  where

import AppConfig

data Env
  = Env
    { appConfig :: AppConfig
    }
  deriving (Show)
