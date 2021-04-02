module API.Telegram where

import API.Telegram.Types
import Control.Exception (handle)
import Control.Monad ((=<<))
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
    ( HttpException(..)
    , Manager(..)
    , Request(..)
    , brRead
    , httpLbs
    , newManager
    , parseRequest
    , responseBody
    , responseStatus
    , withResponse
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)

makeRequest :: MonadThrow m => String -> String -> m Request
makeRequest key method =
    parseRequest $ "https://api.telegram.org/bot" ++ key ++ "/" ++ method

getUpdates :: Manager -> IO L8.ByteString
getUpdates manager = do
    apiKey <- getEnv "TG_API"
    req <- makeRequest apiKey "getUpdates"
    res <- httpLbs req manager
    return $ responseBody res

testMakeRequest = do
    man <- newManager tlsManagerSettings
    getUpdates man
