module Command.Internal.Path
    (userConfigPath
    )
  where


import qualified System.Directory as D
import System.FilePath ((</>))

config_dir = "hh"
config_file = "config.json"

userConfigPath :: IO FilePath
userConfigPath = userConfigPath' config_dir config_file
  where
    userConfigPath' :: FilePath -> FilePath -> IO FilePath
    userConfigPath' path name = do
      dir <- D.getXdgDirectory D.XdgConfig path
      D.createDirectoryIfMissing True dir
      pure $ dir </> name

