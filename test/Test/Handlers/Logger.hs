module Test.Handlers.Logger where

import Data.Text (Text)
import Effects.Log as Log (Priority)
import qualified Handlers.Logger as Logger

new :: Logger.Handle
new = Logger.Handle {Logger.getLog = noLog}

noLog :: (Monad m) => Log.Priority -> Text -> m ()
noLog _ _ = pure ()
