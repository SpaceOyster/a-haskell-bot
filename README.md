# a-haskell-bot
This is a bot for Telegram. All it does is responding to every users message
with it's exact copy multiple times. The 'echo' multiplier can be set globaly,
and individually for every user.
## Commands
Bot can take commands from user:
`/start` - initiates interaction with user;
`/help` - prints help message;
`/repeat` - initiates 'echo' multiplier setting dialog.

## Things to consider
### Telegram
Bot doesn't work in [inline mode](https://core.telegram.org/bots/api#inline-mode)
### Vkontakte
Works only with API version `5.50`
