module Command.Internal.Parser
    ( organizationOption
    , regexOption
    )
  where

import Data.Text
import Options.Applicative

organizationOption :: Parser Text
organizationOption = strOption
    (long "org" <> short 'o' <> metavar "name" <> help "Organization name")

regexOption :: Parser Text
regexOption = strOption
    (long "regex" <> short 'r' <> metavar "regular expression" <> help "Regular expression to filter repositories")

