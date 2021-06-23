module Logger where

log :: String -> IO ()
log = print
data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

