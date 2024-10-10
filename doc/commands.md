# Lambdabot commands

All commands must be prefixed with `@` or `?`, even in private messages.
(Either one may be used.) Lambdabot does completion and edit distance
correction on direct commands, so typoes aren't too critical (but can produce
surprises). ("Direct commands" means what follows `@`. In particular, `@help`
and `@compose` do not do any correction.)

If you want to play, please do so in `/msg` so as to not pollute the channel.
Admins have permission to `@quit` or `/kick` the bot if it gets abused (or, at
their option, to `/kick` the offending user).

## User commands:

```
@@ TEXT
```
- Send TEXT to the channel, with an embedded command expanded
in place. Everything following the command is passed to it,
since it doesn't know how much of what follows should be
parameters.

```
@activity [TIME]
```
- Show bot activity within the past TIME seconds.

```
@all-dicts TERM
@devils TERM
@easton TERM
@elements TERM
@foldoc TERM
@gazetteer TERM
@hitchcock TERM
@jargon TERM
@lojban TERM
@vera TERM
@web1913 TERM
@wn TERM
@world02 TERM
```
- Look up a term in the specified dictionary, or all
dictionaries. Please only do this in `/msg`, as the
result can be very noisy (especially with `@all-dicts`).

```
@arr
@yarr
```
- Talk like a pirate. `@yarr` has a few more phrases.

```
@ask USER TEXT
```
- Leave a message for USER, asking a question.  When
lambdabot sees activity from USER, he/she will be
informed to run `@messages` to retrieve his/her messages.
(Be aware that admins can `@print-messages`, so you may
want to avoid anything too private --- although I would
expect people using public IRC to know that already.)

```
@austro TEXT
@b1ff TEXT
@brooklyn TEXT
@chef TEXT
@cockney TEXT
@drawl TEXT
@dubya TEXT
@fudd TEXT
@funetak TEXT
@jethro TEXT
@jive TEXT
@kraut TEXT
@pansy TEXT
@pirate TEXT
@postmodern TEXT
@redneck TEXT
@valspeak TEXT
@warez TEXT
```
- Exercise GNU textfilters.  Try with `@compose`. (In `/msg`, *please*!)

```
@b52s		
```
- B-52s lyrics quotes.

```
@babel FROM TO TEXT
```
- Ask Babelfish to translate TEXT from one language to
another. Languages: German/DE, Greek/EL, English/EN,
Spanish/ES, French/FR, Italian/IT, Dutch/NL,
Portuguese/PT. (Yes, this is known to be broken at
least some of the time.)

```
@bf TEXT
```
- Run TEXT as a Brainfuck program.

```
@bid SYMBOL ...
```
- Show the combined prices of all stock symbols. Currently broken.

```
@botsnack		
```
- Feed Lambdabot a snack.

```
@brain
```
- Are you thinking what I'm thinking, Pinky?

```
@bug
```
- Show the GHC bug report page.

```
@check EXPR
```
- Run QuickCheck on the specified expression.

```
@clear-messages
```
- Erase all `@messages` for the sender.

```
@clear-topic
```
- Remove the channel topic. Note that the topic is
locked, and lambdabot does not have operator privileges.

```
@compose COMMAND COMMAND
@. COMMAND COMMAND
```
- Feeds output from one command as argument to another.
This can be used to hook `@pl` and `@do` together, or with
"elite" and the GNU talkfilters. It can be chained
(`@. elite . chef b52s`). Note that there is no correction or expansion of
command names with compose, unlike top-level
`@`-commands.

```
@choose STRING ...
```
- Randomly pick an alternative.

```
@define CODE		
@let CODE		
```
- Add a binding, typeclass, pragma, or other
non-expression to lambdabot's environment. Use
`@where L.hs` to see the default environment.
There is no way to dump the current environment.

```
@dequeue-topic
```
- Remove the last item from the channel topic. Note
that the topic is locked, and lambdabot does not
have operator privileges.

```
@dice TYPE
```
- Roll dice.  TYPE is something like `3d6+2` to roll
three 6-sided dice and add 2 to the result.

```
@dict [DICTIONARY]
```
- Brief help for the `@all-dicts` et seq. commands.

```
@dict-help
```
- Lists the dictionaries searched by `@all-dicts` et seq.

```
@djinn TYPE		
```
- Generate code from a non-recursive type. See
https://github.com/lspitzner/exference for a more
capable (but possibly non-terminating) version.

```
@do CODE		
```
- Attempt to rewrite CODE to use `do` notation.

```
@docs
```
- Link to the Haskell documentation.

```
@dummy
```
- Print "dummy". (This isn't quite as silly as it
seems: it's the common code for `@bug`, `@faq`, and other commands.)

```
@elite TEXT
@1337 TEXT
@l33t TEXT
@leet TEXT
```
- Write TEXT in l33tspeak.

```
@eval TEXT
```
- Do nothing, to annoy folks used to other bots.
See `@run` for the actual evaluation command.

```
@faq			
```
- Point to the Haskell FAQ.

```
@forget USER TEXT
```
- Forget a quote (see `@remember`, `@quote`).

```
@fortune		
```
- fortune(1). Be aware that the full database is
installed, including offensive fortunes. Please use `/msg`.

```
@free FUNCTION
```
- Generate a free theorem for a (known) Haskell function.

```
@freshname
```
- Generate a hopefully unused name.

```
@get-shapr
```
- Summon shapr instantly. (Not really.)

```
@get-topic
```
- Show the channel topic.

```
@ghc
```
- Some of ghc's more inscrutable messages.

```
@girl19
```
- I'm not one so can't vouch for these. 😀

```
@google TEXT
```
- Look up TEXT on Google and return the first match.
Currently broken.

```
@googleit TEXT
```
- Use of this command may be grounds for `/kick`.

```
@gsite SITE TEXT
```
- Use Google to look up TEXT on SITE and return the first
match. Currently broken, as above.

```
@gwiki TEXT
```
- Use Google to look up TEXT on the Haskell wiki and
return the first match. Currently broken, as above.

```
@hackage PACKAGE
```
- Show the Hackage URL for a package.

```
@haskellers
```
- Link to a resource for Haskell programmers.

```
@help [COMMAND]
```
- Report what COMMAND does, or suggest `@list` if none.

```
@hoogle TEXT		
```
- Look up a function or a type via Haskell API Search.
See also https://hoogle.haskell.org.

```
@id TEXT
```
- Just print TEXT. Used internally.

```
@index FUNCTION
```
- Show which module(s) export FUNCTION.

```
@instances TYPECLASS
```
- List known instances for a typeclass, with only the Prelude imported.

```
@instances-importing [MODULE ...] TYPECLASS
```
- List known instances for a typeclass, with the specified module(s) imported.

```
@karma USER
@karma+ USER
@karma- USER
```
- Show the current karma for USER, or increase or
decrease their karma explicitly. Lambdabot also
listens for `*word*++` and `*word*--` and adjusts karma.

```
@karma-all
```
- Shows everyone's karma. Noisy; please use in `/msg`.

```
@keal
```
- Quotes from a serial troll.

```
@kind EXPR
:k EXPR
```
- Show the kind of a type. Has access to the same environment as `@run`.

```
@learn
```
- Resources for learning Haskell.

```
@list [MODULE]
```
- List commands provided by a module, or all commands.
(Since the full command list is quite long, bare `@list`
points to this page instead. Besides, it's not exactly
helpful to anyone who isn't familiar with the source.)

```
@listchans
```
- List all connected channels.

```
@listmodules
```
- List all compiled-in modules.

```
@localtime [USER]
@time [USER]
```
- Report the local time for the specified user (or the
user invoking it), using `CTCP TIME`. (Some clients,
notably Pidgin IM, do not respond to `CTCP TIME`.)

```
@messages
```
- Print any messages you've received via `@ask`/`@tell`.
You almost certainly want to do this in `/msg`.

```
@messages?
```
- Check for messages, similar to the check done when
lambdabot is seeing you for the first time on a given
day since a message was left for you.

```
@metar AIRPORT
```
- Show METAR information for the specified airport. Broken.

```
@more
```
- Produce more output (commands will tell you when this is needed).

```
@oeis SEQUENCE
@sequence SEQUENCE
```
- Look up a sequence in the Online Encyclopedia of Integer Sequences.

```
@palomer
```
- Quotes from another serial troll.

```
@paste
```
- Point to a pastebin. Currently this suggests https://paste.debian.org.

```
@ping
```
- Check if the bot is functioning.

```
@pinky
```
- See `@brain`.

```
@pl-resume
```
- Resume attempting to optimize the result of the most recent `@pointless`.

```
@pointless EXPR
@pl EXPR
```
- Convert an expression to point-free form.

```
@poll-list
@poll-show
@poll-add POLL
@choice-add POLL ITEM
@vote POLL CHOICE
@poll-result POLL
@poll-close POLL
@poll-remove POLL
```
- Online polling. Create a poll with `@poll-add`, add
choices with `@choice-add`, vote with `@vote`, see results
with `@poll-result`. `@poll-close` prevents voting on a
poll. (Polls cannot be reopened.)

```
@pop-topic
```
- Remove the last item from the channel topic. Note
that the topic is locked, and lambdabot does not have
operator privileges.

```
@pretty EXPR
```
- Pretty-print a Haskell expression.

```
@push-topic TEXT	
```
- Append text to the channel topic, with a separator. Note
that the topic is locked, and lambdabot does not have
operator privileges.

```
@queue-topic TEXT
```
- Prepend TEXT to the channel topic, with a separator. Note
that the topic is locked, and lambdabot does not have
operator privileges.

```
@quote [[USER] REGEXP]
```
- Produce a random quote optionally matching a (POSIX BRE)
regexp and optionally by a specified user.

```
@remember USER TEXT
```
- Remember a quote.

```
@run EXPR
> EXPR
```
- Evaluate a Haskell expression. The default environment
for evaluation is shown by `@where L.hs`. Additions
can be made with `@define`, or reset by `@undefine`.

```
@seen [USER]
```
- Report where and when a user was last seen.  May
also take a channel, in which case it will respond privately.

```
@set-topic TEXT
```
- Set the channel topic. Note that the topic is
locked, and lambdabot does not have operator privileges.

```
@shift-topic
```
- Remove the first element from the channel topic.
Note that the topic is locked, and lambdabot does
not have operator privileges.

```
@shootout
```
- The Debian Benchmarks Game, formerly the Debian Language Shootout.

```
@show TEXT
```
- Print TEXT as modified by Haskell's `show`

```
@slap USER
```
- Might or might not abuse the specified user.

```
@spell WORD
```
- Look up WORD with aspell.

```
@spell-all TEXT
```
- Check spelling of TEXT.

```
@src [TYPECLASS] FUNCTION
```
- Show the definition of a function or operator. This
comes from a curated database mostly built up from
the Haskell Report, and therefore may be incomplete
or inaccurate.

```
@tell USER TEXT
```
- Leave a message for USER. The only difference between
this and `@ask` is that `@messages` says `USER says: TEXT`
instead of `USER asks: TEXT`. (Be aware that admins
can `@print-messages`, so you may want to avoid anything
too private --- although I would expect people using
public IRC to know that already.)

```
@thanks
@thank-you
@thx
```
- Thank the bot.

```
@tic-tac-toe
```
- How about a nice game of chess?

```
@ticker SYMBOL ...
```
- Show the price of one or more stocks. Currently broken.

```
@todo
```
- Print the to-do list. Very obsolete currently.

```
@todo-add TEXT
```
- Append an item to the to-do list.

```
@type EXPR
:t EXPR
```
- Show the type of an expression. Has access to the
same environment as `@run`.

```
@undo EXPR
```
- Convert `do` expressions to equivalents using `(>>=)`
and `(>>)`. Often best combined with `@pl` using
`@compose` (`@. pl undo EXPR`).

```
@unlambda TEXT
```
- Evaluate an Unlambda expression.

```
@unmtl TYPE
```
- Show the unwrapped version of an MTL type.

```
@unshift-topic TEXT
```
- Prepend TEXT to the channel topic, with a separator.
Note that the topic is locked, and lambdabot does not
have operator privileges.

```
@undefine		
```
- Reset the environment for `@run`, `@type`, etc. to the
default (see `@define`).

```
@uptime
```
- Report lambdabot's uptime.

```
@url THING
@what THING
@where THING
```
- Report the location of THING from the `@where` database.

```
@users [CHANNEL]
```
- Show the number of users and most recent activity in
the current or a specified channel.

```
@v
```
- Lambdabot used to use an internal binding `v` when
doing `@run`. This is some quotes from abusing it and
Haskell's default "letrec".

```
@version
```
- Report the upstream version, author, and build information.

```
@where+ THING TEXT
```
- Add an entry to the `@url`/`@where` database. If TEXT
is omitted, remove THING instead.

```
@wiki
```
- Point to the Haskell wiki.

```
@yhjulwwiefzojcbxybbruweejw
```
- What happened after `v` got renamed (see `@v`).

```
@yow
```
- Zippy the Pinhead.

## Admin commands:

These may only be invoked by bot administrators.

```
@activity-full [TIME]
```
- Show all bot activity within TIME seconds.

```
@admin {+|-} SERVER:USER	
```
- Add or remove USER on SERVER from the admin list.

```
@echo TEXT		
```
- Print TEXT as raw IRC protocol.

```
@flush
```
- Flush all state to disk.

```
@ignore {+|-} SERVER:USER
```
- Add or remove USER on SERVER from the ignore list.
All commands from that user will be ignored.  Karma
and URL lookups will *not* currently be ignored.

```
@irc-connect SERVER HOST PORT USER USERINFO
```
- Establish a connection to an IRC server, identified
by tag SERVER which is used by commands like `@admin`.
USERINFO should include the bot's maintainer.

```
@join [SERVER:]CHANNEL
```
- Join the specified channel.

```
@leave [SERVER:]CHANNEL
@part [SERVER:]CHANNEL
```
- Leave the specified server/channel

```
@listall
```
- List all (user) commands, instead of pointing here.
(This currently trips over the very short command
execution timeout and aborts before finishing.  You
probably don't want to use this anyway, since this
page is far better; the original COMMANDS page was
just the output of `@listall`, and as such was nearly
useless.)

```
@msg [SERVER:]CHANNEL TEXT
```
- Send the specified text to a channel/user (as the bot).
Intended for e.g. NickServ password.

```
@nazi-on [CHANNEL]
@nazi-off [CHANNEL]
```
- Spelling-nazi mode (pass everything on the channel to
`@spell-all`).  Extremely annoying, to the point that I
might be tempted to kickban anyone who invokes it.

```
@offline
```
- Start an offline session.  Should only work from the console.

```
@print-notices
```
- List all the `@messages` in the system.

```
@purge-messages [USER]
```
- Purge USER's or *all* `@messages` in the system.

```
@quit [REASON]
```
- Shut down the bot, optionally specifying a reason

```
@rc FILE
```
- Read a file of commands.  Should only work from the console.

```
@reconnect
```
- Reconnect to the IRC server.

```
@state [EXPR]		
```
- Examine internal state.  Not useful without Lambdabot source code.

```
@todo-delete ITEM
```
- Delete ITEM (by list position) from the to-do list.
