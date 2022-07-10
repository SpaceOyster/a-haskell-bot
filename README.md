# a-haskell-bot
This is a bot for Telegram and Vkontakte. All it does is responding to every
users message with it's exact copy multiple times. The 'echo' multiplier can be
set globaly, and individually for every user.
## Compilation
```bash
stack build
stack install
```
## Usage
```bash
a-haskell-bot BOT FILE
```

Available `BOT`s:
  telegram - run bot for Telegram
  vkontakte - run bot for Vkontakte
  
`FILE` is a `JSON` formatted config file

Usage example:
```bash
a-haskell-bot telegram config.json
```

## Configuration
```json
{
  "defaults": {
    "default-echo-multiplier": 1
  },
  "replies": {
    "help": "This is a help message.",
    "greeting": "Hello. This bot echoes your messages multiple times.\nUse '/help' command to see available commands list.",
    "repeat": "How many times you want your messages to be repeated?",
    "unknown": "Unknown command. Use '/help' to print commands list.",
    "settings-saved": "Settings successfully updated."
  },
  "logger": {
    "file": "../bot-log-file",
    "verbosity": "debug"
  },
  "telegram": {
    "api-key": "0123456789abcdefghigklmnopqrstuvwxyz",
    "timeout-seconds": 50
  },
  "vkontakte": {
    "api-key": "0123456789abcdefghigklmnopqrstuvwxyz",
    "group-id": 123456789,
    "api-version": "5.86",
    "wait-seconds": 50
  }
}
```

## Commands
Bot can take commands from user:
`/start` - initiates interaction with user;
`/help` - prints help message;
`/repeat` - initiates 'echo' multiplier setting dialog.

## Things to consider
### Telegram

Bot doesn't work in [inline mode](https://core.telegram.org/bots/api#inline-mode)

### Vkontakte

Works only with API version `5.86`
