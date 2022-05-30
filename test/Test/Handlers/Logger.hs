module Test.Handlers.Logger where

import Effects.Log as Log
import qualified Handlers.Logger as Logger
import Data.Text (Text)

new :: Logger.Handle
new = Logger.Handle {Logger.getLog = noLog}

noLog :: (Monad m) => Log.Priority -> Text -> m ()
noLog _ _ = pure ()
