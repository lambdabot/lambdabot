{- Leave a message with lambdabot, the faithful secretary

> 17:11 < davidhouse> @tell dmhouse foo
> 17:11 < hsbot> Consider it noted
> 17:11 < davidhouse> @tell dmhouse bar
> 17:11 < hsbot> Consider it noted
> 17:11 < dmhouse> hello!
> 17:11 < hsbot> dmhouse: You have 2 new messages. '/msg hsbot @messages' to read them.
> 17:11 < dmhouse> Notice how I'm speaking again, and hsbot isn't buzzing me more than that one time.
> 17:12 < dmhouse> It'll buzz me after a day's worth of not checking my messages.
> 17:12 < dmhouse> If I want to check them in the intermittent period, I can either send a /msg, or:
> 17:12 < dmhouse> @messages?
> 17:12 < hsbot> You have 2 messages
> 17:12 < dmhouse> Let's check them, shall we?
>
> [In a /msg to hsbot]
> 17:12 <hsbot> davidhouse said less than a minute ago: foo
> 17:12 <hsbot> davidhouse said less than a minute ago: bar
>
> [Back in the channel
> 17:12 < dmhouse> You needn't use a /msg, however. If you're not going to annoy the channel by printing 20 of
>                  your messages, feel free to just type '@messages' in the channel.
> 17:12 < davidhouse> @tell dmhouse foobar
> 17:12 < hsbot> Consider it noted
> 17:12 < davidhouse> @ask dmhouse barfoo
> 17:12 < hsbot> Consider it noted
> 17:12 < davidhouse> You can see there @ask. It's just a synonym for @tell, but it prints "foo asked X ago M",
>                     which is more natural. E.g. '@ask dons whether he's applied my latest patch yet?'
> 17:13 < dmhouse> For the admins, a useful little debugging tool is @print-notices.
> 17:13 < hsbot> dmhouse: You have 2 new messages. '/msg hsbot @messages' to read them.
> 17:14 < dmhouse> Notice that hsbot pinged me there, even though it's less than a day since I last checked my
>                  messages, because there have been some new ones posted.
> 17:14 < dmhouse> @print-notices
> 17:14 < hsbot> {"dmhouse":=(Just Thu Jun  8 17:13:46 BST 2006,[Note {noteSender = "davidhouse", noteContents =
>                "foobar", noteTime = Thu Jun  8 17:12:50 BST 2006, noteType = Tell},Note {noteSender =
                 "davidhouse", noteContents = "barfoo", noteTime = Thu Jun  8 17:12:55 BST 2006, noteType = Ask}])}
> 17:15 < dmhouse> There you can see the two notes. The internal state is a map from recipient nicks to a pair of
>                  (when we last buzzed them about having messages, a list of the notes they've got stacked up).
> 17:16 < dmhouse> Finally, if you don't want to bother checking your messages, then the following command will
>                  likely be useful.
> 17:16 < dmhouse> @clear-messages
> 17:16 < hsbot> Messages cleared.
> 17:16 < dmhouse> That's all, folks!
> 17:17 < dmhouse> Any comments, queries or complaints to dmhouse@gmail.com. The source should be fairly readable, so
>                  hack away!
-}

module Lambdabot.Plugin.Tell (theModule) where

import Lambdabot.Compat.FreenodeNick
import Lambdabot.Plugin
import Lambdabot.Compat.AltTime

import Control.Arrow (first)
import Control.Monad
import qualified Data.Map as M
import Text.Printf (printf)

-- | Was it @tell or @ask that was the original command?
data NoteType    = Tell | Ask deriving (Show, Eq, Read)
-- | The Note datatype. Fields self-explanatory.
data Note        = Note { noteSender   :: FreenodeNick,
                          noteContents :: String,
                          noteTime     :: ClockTime,
                          noteType     :: NoteType }
                   deriving (Eq, Show, Read)
-- | The state. A map of (times we last told this nick they've got messages, the
--   messages themselves)
type NoticeBoard = M.Map FreenodeNick (Maybe ClockTime, [Note])

type Tell = ModuleT NoticeBoard LB

theModule :: Module NoticeBoard
theModule = newModule
    { moduleCmds = return
        [ (command "tell")
            { help = say "tell <nick> <message>. When <nick> shows activity, tell them <message>."
            , process = doTell Tell . words
            }
        , (command "ask")
            { help = say "ask <nick> <message>. When <nick> shows activity, ask them <message>."
            , process = doTell Ask . words
            }
        , (command "messages")
            { help = say "messages. Check your messages, responding in private."
            , process = const (doMessages False)
            }
        , (command "messages-loud")
            { help = say "messages. Check your messages, responding in public."
            , process = const (doMessages True)
            }
        , (command "messages?")
            { help = say "messages?. Tells you whether you have any messages"
            , process = const $ do
                sender <- getSender
                ms <- getMessages sender
                case ms of
                    Just _ -> doRemind sender
                    Nothing   -> say "Sorry, no messages today."
            }
        , (command "clear-messages")
            { help = say "clear-messages. Clears your messages."
            , process = const $ do
                sender <- getSender
                clearMessages sender
                say "Messages cleared."
            }
        , (command "print-notices")
            { privileged = True
            , help = say "print-notices. Print the current map of notes."
            , process = const ((say . show) =<< readMS)
            }
        , (command "purge-notices")
            { privileged = True
            , help = say $
                "purge-notices [<nick> [<nick> [<nick> ...]]]]. "
                ++ "Clear all notes for specified nicks, or all notices if you don't "
                ++ "specify a nick."
            , process = \args -> do
                users <- mapM readNick (words args)
                if null users
                    then writeMS M.empty
                    else mapM_ clearMessages users
                say "Messages purged."
            }
        ]
    , moduleDefState  = return M.empty
    , moduleSerialize = Just mapSerial
    -- Hook onto contextual. Grab nicks of incoming messages, and tell them
    -- if they have any messages, if it's less than a day since we last did so.
    , contextual = const $ do
        sender <- getSender
        remp <- needToRemind sender
        if remp
            then doRemind sender
            else return ()
    }

-- | Take a note and the current time, then display it
showNote :: ClockTime -> Note -> Cmd Tell String
showNote time note = do
    sender <- showNick (getFreenodeNick (noteSender note))
    let diff    = time `diffClockTimes` noteTime note
        ago     = case timeDiffPretty diff of
                    [] -> "less than a minute"
                    pr -> pr
        action  = case noteType note of Tell -> "said"; Ask -> "asked"
    return $ printf "%s %s %s ago: %s" sender action ago (noteContents note)

-- | Is it less than a day since we last reminded this nick they've got messages?
needToRemind :: Nick -> Cmd Tell Bool
needToRemind n = do
  st  <- readMS
  now <- io getClockTime
  return $ case M.lookup (FreenodeNick n) st of
             Just (Just lastTime, _) ->
               let diff = now `diffClockTimes` lastTime
               in diff > TimeDiff 86400
             Just (Nothing,       _) -> True
             Nothing                 -> True

-- | Add a note to the NoticeBoard
writeDown :: Nick -> Nick -> String -> NoteType -> Cmd Tell ()
writeDown to from what ntype = do
  time <- io getClockTime
  let note = Note { noteSender   = FreenodeNick from,
                    noteContents = what,
                    noteTime     = time,
                    noteType     = ntype }
  modifyMS (M.insertWith (\_ (_, ns) -> (Nothing, ns ++ [note]))
                         (FreenodeNick to) (Nothing, [note]))

-- | Return a user's notes, or Nothing if they don't have any
getMessages :: Nick -> Cmd Tell (Maybe [Note])
getMessages sender = fmap (fmap snd . M.lookup (FreenodeNick sender)) readMS

-- | Clear a user's messages.
clearMessages :: Nick -> Cmd Tell ()
clearMessages sender = modifyMS (M.delete (FreenodeNick sender))

-- * Handlers
--

-- | Give a user their messages
doMessages :: Bool -> Cmd Tell ()
doMessages loud = do
    sender <- getSender
    msgs <- getMessages sender
    clearMessages sender

    let tellNote = if loud
            then say
            else lb . ircPrivmsg sender

    case msgs of
        Nothing -> say "You don't have any messages"
        Just mesgs -> do
            time <- io getClockTime
            mapM_ (showNote time >=> tellNote) mesgs

verb :: NoteType -> String
verb Ask = "ask"
verb Tell= "tell"

-- | Execute a @tell or @ask command.
doTell :: NoteType -> [String] -> Cmd Tell ()
doTell ntype []         = say ("Who should I " ++ verb ntype ++ "?")
doTell ntype (who:args) = do
    recipient <- readNick who
    sender <- getSender
    me <- getLambdabotName
    let rest = unwords args
        (record, res) | sender    == recipient   = (False, "You can " ++ verb ntype ++ " yourself!")
            | recipient == me          = (False, "Nice try ;)")
            | null args                = (False, "What should I " ++ verb ntype ++ " " ++ who ++ "?")
            | otherwise                = (True,  "Consider it noted.")
    when record (writeDown recipient sender rest ntype)
    say res

-- | Remind a user that they have messages.
doRemind :: Nick -> Cmd Tell ()
doRemind sender = do
    ms  <- getMessages sender
    now <- io getClockTime
    modifyMS (M.update (Just . first (const $ Just now)) (FreenodeNick sender))
    case ms of
        Just msgs -> do
           me <- showNick =<< getLambdabotName
           let (messages, pronoun)
                   | length msgs > 1   = ("messages", "them")
                   | otherwise         = ("message", "it")
               msg = printf "You have %d new %s. '/msg %s @messages' to read %s."
                       (length msgs) messages me pronoun
           lb (ircPrivmsg sender msg)
        Nothing -> return ()
