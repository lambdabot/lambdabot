{- | Leave a message with lambdabot, the faithful secretary.

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
> 17:12 < davidhouse> @tell dmhouse foobar
> 17:12 < hsbot> Consider it noted
> 17:12 < davidhouse> @tell dmhouse barfoo
> 17:12 < hsbot> Consider it noted
> 17:13 < dmhouse> For the admins, a useful little debugging tool is @print-notices.
> 17:13 < hsbot> dmhouse: You have 2 new messages. '/msg hsbot @messages' to read them.
> 17:14 < dmhouse> Notice that hsbot pinged me there, even though it's less than a day since I last checked my
>                  messages, because there have been some new ones posted.
> 17:14 < dmhouse> @print-notices
> 17:14 < hsbot> {"dmhouse":=(Just Thu Jun  8 17:13:46 BST 2006,[Note {noteSender = "davidhouse", noteContents =
>                "foobar", noteTime = Thu Jun  8 17:12:50 BST 2006},Note {noteSender = "davidhouse", noteContents = "
> 17:14 < hsbot> barfoo", noteTime = Thu Jun  8 17:12:55 BST 2006}])}
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

module Plugin.Tell where

import Control.Arrow (first)
import qualified Data.Map as M
import System.Time

import Message (nick, channels)
import Plugin

type Nick        = String
data Note        = Note { noteSender   :: String, 
                          noteContents :: String, 
                          noteTime     :: ClockTime }
                   deriving (Eq, Show{-, Read-})
-- Store a list of (times we last told this nick they've got messages, the
-- messages themselves)
type NoticeBoard = M.Map Nick (Maybe ClockTime, [Note])
type Telling a   = ModuleT NoticeBoard LB a 

PLUGIN Tell

instance Module TellModule NoticeBoard where
    moduleCmds      _ = ["tell", "messages", "messages?", "clear-messages"]
    modulePrivs     _ = ["print-notices"]
    moduleHelp _      = fromJust . flip lookup help
    moduleDefState  _ = return M.empty
    -- moduleSerialize _ = Just mapSerial -- ClockTime doesn't instantiate Read

    -- | Debug output the NoticeBoard
    process _ _ _ "print-notices" _ = liftM ((:[]) . show) readMS

    -- | Clear a user's notes
    process _ msg _ "clear-messages" _ = 
      clearMessages (nick msg) >> return ["Messages cleared."]

    -- | Check whether a user has any messages
    process _ msg _ "messages?" _  = do
      ms <- getMessages (nick msg)
      case ms of
        Just msgs -> return ["You have " ++ (show $ length msgs) ++ " messages"]
        Nothing   -> return ["Sorry, no messages today."]

    -- | Write down a note
    process _ msg _ "tell" args = do 
      let args'     = words args
          recipient = head args'
          sender    = nick msg
          rest      = unwords $ tail args'
          res | sender    == recipient   = Left "You can tell yourself!"
              | recipient == name config = Left "Nice try ;)"
              | otherwise                = Right "Consider it noted"
      when (isRight res) (writeDown recipient sender rest)
      return [unEither res]

    -- | Give a user their messages
    process _ msg _ "messages" _ =
      if any ((=='#') . head) (channels msg)
         then return ["You can only use @messages in a /msg"]
         else do msgs <- getMessages $ nick msg
                 let res = fromMaybe ["You don't have any new messages."] msgs
                 clearMessages (nick msg)
                 return res

    -- | Hook onto contextual. Grab nicks of incoming messages, and tell them
    --   if they have any messages, if it's less than a day since we last did so.
    contextual _ msg _ _ = do 
      let sender = nick msg
      remind <- needToRemind sender
      if remind
         then do
           ms  <- getMessages sender
           now <- io getClockTime
           modifyMS (M.update (Just . first (const $ Just now)) sender)
           return $ maybe [] (\msgs ->
             [sender ++ ": You have " ++ (show $ length msgs) ++
              " new messages. '/msg " ++  (name config) ++ 
              " @messages' to read them."]) ms
         else return []

-- | Lookup table for documentation
help :: [(String, String)]
help = [("tell",
         "tell <nick> <message>. When <nick> shows activity, tell them " ++
           "<message>."),
        ("messages",
         "messages. Check your messages."),
        ("messages?",
         "messages?. Tells you whether you have any messages"),
        ("clear-messages",
         "clear-messages. Clears your messages."),
        ("print-notices", "Print the current map of notes.") ]

-- | Take a note and the current time, then display it
showNote :: ClockTime -> Note -> String
showNote time note = noteSender note ++ " said " ++ ago ++ " ago: " ++ noteContents note
    where diff         = time `diffClockTimes` noteTime note
          diff'        = normalizeTimeDiff diff
          mkAgo td msg = if td > 0 then Just (show td ++ msg) else Nothing
          agos         = [ mkAgo (tdYear  diff') " years",
                           mkAgo (tdMonth diff') " months",
                           mkAgo (tdDay   diff') " days",
                           mkAgo (tdHour  diff') " hours",
                           mkAgo (tdMin   diff') " minutes" ]
          agos'        = catMaybes agos
          ago          = if null agos'
                           then "less than a minute"
                           else concatList agos'

-- | Is it less than a day since we last reminded this nick they've got messages?
needToRemind :: Nick -> Telling Bool
needToRemind n = do
  st  <- readMS
  now <- io getClockTime
  return $ case M.lookup n st of
             Just (Just lastTime, _) -> 
               let diff = now `diffClockTimes` lastTime
               in diff > noTimeDiff { tdDay = 1 }
             Just (Nothing,       _) -> True
             Nothing                 -> True

-- | Add a note to the NoticeBoard
writeDown :: Nick -> Nick -> String -> Telling ()
writeDown to from what = do 
  time <- io getClockTime
  let note = Note { noteSender   = from, 
                    noteContents = what, 
                    noteTime     = time }
  modifyMS (M.insertWith (\_ (_, ns) -> (Nothing, ns ++ [note]))
                         to (Nothing, [note]))

-- | Return a user's notes, or Nothing if they don't have any
getMessages :: Nick -> Telling (Maybe [String])
getMessages n = do 
  st   <- readMS
  time <- io getClockTime
  case M.lookup n st of
    Just (_, msgs) -> do
      -- update the last time we told this person they had messages
      writeMS $ M.insert n (Just time, msgs) st
      return . Just $ map (showNote time) msgs
    Nothing -> return Nothing

-- | Clear a user's messages
clearMessages :: Nick -> Telling ()
clearMessages n = modifyMS (M.delete n) -- pointfree won't work with impredicativity (?)
