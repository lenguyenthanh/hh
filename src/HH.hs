module HH
    (hh
    )
  where

import App
import Command
import Command.CloneRepos
import Command.Create
import Command.InitConfig
import Command.ShowRepos

hh :: IO ()
hh = runAppM app ()

--app :: (Monad m, MonadCommand m) => AppM m ()
app :: AppM IO ()
app = do
  command <- getCommand
  runCommand command

runCommand :: Command -> AppM IO ()
runCommand (Init args) = runSaveConfig args
runCommand ShowConfig = runShowConfig
runCommand (ShowRepos args) = runShowRepos args
runCommand (CloneRepos args) = runCloneRepos args
runCommand (Create args) = runCreate args
