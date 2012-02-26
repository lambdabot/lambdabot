--
-- | The IRC module processes the IRC protocol and provides a nice API for sending
--   and recieving IRC messages with an IRC server.
--
module IRCBase ( IrcMessage(..)
               , privmsg
               , quit
               , timeReply
               , errShowMsg -- TODO: remove
               , user
               , setNick
               ) where

import Message
import Lambdabot.Util (split, breakOnGlue, clean)
import qualified Lambdabot.Util as Util (concatWith)

import Data.Char (chr,isSpace)

import Control.Monad (liftM2)

-- | An IRC message is a server, a prefix, a command and a list of parameters.
data IrcMessage
  = IrcMessage {
        msgServer   :: !String,
        msgLBName   :: !String,
        msgPrefix   :: !String,
        msgCommand  :: !String,
        msgParams   :: ![String]
  }
  deriving (Show)

instance Message IrcMessage where
    nick          = IRCBase.nick
    server        = IRCBase.msgServer
    fullName      = IRCBase.fullName
    names         = IRCBase.names
    channels      = IRCBase.channels
    joinChannel   = IRCBase.join
    partChannel   = IRCBase.part
    getTopic      = IRCBase.getTopic
    setTopic      = IRCBase.setTopic
    body          = IRCBase.msgParams
    command       = IRCBase.msgCommand
    lambdabotName = IRCBase.lambdabotName

-- | 'mkMessage' creates a new message from a server, a cmd, and a list of parameters.
mkMessage :: String -- ^ Server
          -> String -- ^ Command
          -> [String] -- ^ Parameters
          -> IrcMessage -- ^ Returns: The created message

mkMessage svr cmd params = IrcMessage { msgServer = svr, msgPrefix = "", msgCommand = cmd, msgParams = params,
                                        msgLBName = "urk!<outputmessage>" }

-- | 'nick' extracts the nickname involved in a given message.
nick :: IrcMessage -> Nick
nick = liftM2 Nick msgServer (fst . breakOnGlue "!" . msgPrefix)

-- | 'fullName' extracts the full user name involved in a given message.
fullName :: IrcMessage -> String
fullName = snd . breakOnGlue "!" . msgPrefix

-- | 'channels' extracts the channels a IrcMessage operate on.
channels :: IrcMessage -> [Nick]
channels msg
  = let cstr = head $ msgParams msg
    in map (Nick (msgServer msg)) $
       map (\(x:xs) -> if x == ':' then xs else x:xs) (split "," cstr)
           -- solves what seems to be an inconsistency in the parser

-- | 'privmsg' creates a private message to the person designated.
privmsg :: Nick -- ^ Who should recieve the message (nick)
        -> String -- ^ What is the message?
        -> IrcMessage -- ^ Constructed message
privmsg who msg = if action then mk [nName who, ':':(chr 0x1):("ACTION " ++ clean_msg ++ ((chr 0x1):[]))]
                            else mk [nName who, ':' : clean_msg]
    where mk = mkMessage (nTag who) "PRIVMSG"
          cleaned_msg = case concatMap clean msg of
              str@('@':_) -> ' ':str
              str         -> str
          (clean_msg,action) = case cleaned_msg of
              ('/':'m':'e':r) -> (dropWhile isSpace r,True)
              str             -> (str,False)

-- | 'setTopic' takes a channel and a topic. It then returns the message
--   which sets the channels topic.
setTopic :: Nick -- ^ Channel
         -> String -- ^ Topic
         -> IrcMessage
setTopic chan topic = mkMessage (nTag chan) "TOPIC" [nName chan, ':' : topic]

-- | 'getTopic' Returns the topic for a channel, given as a String
getTopic :: Nick -> IrcMessage
getTopic chan = mkMessage (nTag chan) "TOPIC" [nName chan]

-- | 'quit' creates a server QUIT message. The input string given is the
--   quit message, given to other parties when leaving the network.
quit :: String -> String -> IrcMessage
quit svr msg = mkMessage svr "QUIT" [':' : msg]

-- | 'join' creates a join message. String given is the location (channel)
--   to join.
join :: Nick -> IrcMessage
join loc = mkMessage (nTag loc) "JOIN" [nName loc]

-- | 'part' parts the channel given.
part :: Nick -> IrcMessage
part loc = mkMessage (nTag loc) "PART" [nName loc]

-- | 'names' builds a NAMES message from a list of channels.
names :: String -> [String] -> IrcMessage
names svr chans = mkMessage svr "NAMES" [Util.concatWith "," chans]

-- | Construct a privmsg from the CTCP TIME notice, to feed up to
-- the @localtime-reply plugin, which then passes the output to
-- the appropriate client.
timeReply :: IrcMessage -> IrcMessage
timeReply msg    =
   IrcMessage { msgPrefix  = msgPrefix (msg)
              , msgServer  = msgServer (msg)
              , msgLBName  = msgLBName (msg)
              , msgCommand = "PRIVMSG"
              , msgParams  = [head (msgParams msg)
                             ,":@localtime-reply " ++ (nName $ IRCBase.nick msg) ++ ":" ++
                                (init $ drop 7 (last (msgParams msg))) ]
              }

-- Only needed for Base.hs
errShowMsg :: IrcMessage -> String
errShowMsg msg = "ERROR> <" ++ msgServer msg ++ (':' : msgPrefix msg) ++
      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)

user :: String -> String -> String -> String -> IrcMessage
user svr nick_ server_ ircname = IRCBase.mkMessage svr "USER" [nick_, "localhost", server_, ircname]

setNick :: Nick -> IrcMessage
setNick nick_ = IRCBase.mkMessage (nTag nick_) "NICK" [nName nick_]

lambdabotName :: IrcMessage -> Nick
lambdabotName msg = Nick (msgServer msg) (msgLBName msg)
