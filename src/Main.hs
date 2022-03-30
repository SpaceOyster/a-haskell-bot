{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import App.Config
import qualified App.Env as App
import qualified App.Monad as App
import qualified Bot
import qualified Bot.Replies as Bot
import qualified Bot.Telegram as TG
import qualified Bot.Vkontakte as VK
import Control.Applicative ((<|>))
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as A (decode)
import Control.Monad.Catch (SomeException, catchAll)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Text.Extended as T
import qualified HTTP
import qualified Logger
import qualified System.Environment as E
import qualified System.Exit as Exit (die)
import qualified UsersDB

data BotToRun
  = Telegram
  | Vkontakte

main :: IO ()
main = initiateAndRun <|> Exit.die usagePrompt
  where
    initiateAndRun :: IO ()
    initiateAndRun = do
      bot : cfgPath : _as <- E.getArgs
      cfg <- readConfig cfgPath
      botToRun <- readBotName bot
      runWithApp cfg botToRun `catchAll` uncaughtExceptions
    uncaughtExceptions :: SomeException -> IO ()
    uncaughtExceptions e = do
      putStrLn . ("Uncaught Exception: " <>) . show $ e
      Exit.die "\nClosing application."

readBotName :: (MonadFail m) => String -> m BotToRun
readBotName str =
  case toLower <$> str of
    "telegram" -> pure Telegram
    "vkontakte" -> pure Vkontakte
    _ -> fail "unknown bot type"

readConfig :: FilePath -> IO AppConfig
readConfig cfgPath = do
  json <- BL.readFile cfgPath
  case A.decode json of
    Just cfg -> pure cfg
    Nothing -> let str = "Unable to parse App Config\n" in putStrLn str >> fail str

usagePrompt :: String
usagePrompt =
  intercalate
    "\n"
    [ "a-haskell-bot - Echo bot for Telegram and Vkontakte, written in Haskell",
      mempty,
      "Usage: a-haskell-bot BOT FILE",
      mempty,
      "FILE - is a config file formatted as json, see example config here:",
      mempty,
      "Available bots:",
      "  telegram - run bot for Telegram",
      "  vkontakte - run bot for Vkontakte ",
      "INSTRUCTIONS TO FIND EXAMPLE CONFIG"
    ]

runWithApp :: AppConfig -> BotToRun -> IO ()
runWithApp AppConfig {..} bot =
  Logger.withHandle logger $ \hLog -> do
    Logger.logInfo hLog "Initiating Main Bot loop"
    Logger.logInfo hLog $
      "API Polling period is "
        <> T.tshow (fromIntegral poll_period / 1000 :: Double)
        <> "ms"
    hHTTP <- HTTP.new HTTP.Config {}
    hUsersDB <- UsersDB.new UsersDB.Config {defaultEchoMultiplier}
    let env =
          App.Env
            { envLogger = hLog,
              envHTTP = hHTTP,
              envUsersDB = hUsersDB,
              envBotReplies = Bot.fromRepliesM repliesM
            }
    let app =
          case bot of
            Telegram -> TG.evalTelegramT telegram $ Bot.loop poll_period
            Vkontakte -> VK.evalVkontakteT vkontakte $ Bot.loop poll_period
    App.unApp app `runReaderT` env
