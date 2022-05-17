{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module App.Main where

import App.Config
  ( AppConfig
      ( defaultEchoMultiplier,
        logger,
        replies,
        telegramM,
        vkontakteM
      ),
  )
import qualified App.Env as App
import App.Error (botError)
import qualified App.Monad as App
import qualified Bot
import qualified Bot.Telegram as TG
import qualified Bot.Vkontakte as VK
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow, SomeException, catchAll, throwM)
import qualified Data.Aeson as Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Text.Extended as T (tshow)
import qualified Effects.BotReplies ()
import qualified Handlers.HTTP as HTTP
import qualified Handlers.Logger as Logger
import qualified Handlers.UsersDB as UsersDB
import qualified System.Environment as E
import qualified System.Exit as Exit (die)

data BotToRun
  = Telegram
  | Vkontakte
  deriving (Show)

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
    uncaughtExceptions e =
      Exit.die $ "Uncaught Exception: " <> show e <> "\nClosing application."

readBotName :: (MonadFail m) => String -> m BotToRun
readBotName str =
  case toLower <$> str of
    "telegram" -> pure Telegram
    "vkontakte" -> pure Vkontakte
    _ -> fail "Unknown bot type"

readConfig :: FilePath -> IO AppConfig
readConfig cfgPath = do
  json <- BL.readFile cfgPath
  case Aeson.eitherDecode json of
    Right cfg -> pure cfg
    Left err -> putStrLn (failPrompt err) >> fail (failPrompt err)
  where
    failPrompt :: String -> String
    failPrompt e = "Unable to parse App Config: \n\t" <> e <> "\n"

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
runWithApp cfg bot =
  Logger.withHandle (logger cfg) $ \hLog -> do
    Logger.hLogInfo hLog "Initiating Main Bot loop"
    env <- newAppEnv hLog cfg
    app <- interpretWith bot cfg Bot.botLoop
    App.runApp env app

newAppEnv :: Logger.Handle -> AppConfig -> IO App.AppEnv
newAppEnv hLog appCfg = do
  let defaultEchoMultiplier = App.Config.defaultEchoMultiplier appCfg
  hHTTP <- HTTP.new HTTP.Config {}
  hUsersDB <- UsersDB.new UsersDB.Config {defaultEchoMultiplier}
  pure
    App.Env
      { envLogger = hLog,
        envHTTP = hHTTP,
        envUsersDB = hUsersDB,
        envBotReplies = replies appCfg
      }

interpretWith :: (MonadThrow m) => BotToRun -> AppConfig -> Bot.BotScript ret -> m (App.App ret)
interpretWith bot cfg script =
  case bot of
    Telegram -> interpretWithMaybe TG.evalTelegramBot (telegramM cfg)
    Vkontakte -> interpretWithMaybe VK.evalVkontakteBot (vkontakteM cfg)
  where
    ex = throwM . botError $ "No config is present for bot: " <> T.tshow bot
    interpretWithMaybe eval maybeConfig = do
      botCfg <- maybe ex pure maybeConfig
      pure . eval botCfg . Bot.interpret $ script
