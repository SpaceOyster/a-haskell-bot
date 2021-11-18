{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Handle.Class
    ( IsHandle(..)
    ) where

import qualified Logger

class IsHandle h cfg | cfg -> h where
    new :: cfg -> Logger.Handle -> IO h
    withHandle :: cfg -> Logger.Handle -> (h -> IO a) -> IO a
    withHandle config hLog io = do
        hBot <- new config hLog
        io hBot
