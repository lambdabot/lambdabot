--
-- | The IRC module processes the IRC protocol and provides a nice API for sending
--   and recieving IRC messages with an IRC server.
--
module Lambdabot.IRC
    ( IrcMessage(..)
    , joinChannel
    , partChannel
    , getTopic
    , setTopic
    , privmsg
    , quit
    , timeReply
    , errShowMsg -- TODO: remove
    , user
    , setNick
    ) where

import Lambdabot.Message
import Lambdabot.Nick

import Data.Char (chr,isSpace)
import Data.List.Split

import Control.Monad (liftM2)

-- | An IRC message is a server, a prefix, a command and a list of parameters.
data IrcMessage
  = IrcMessage {
        ircMsgServer   :: !String,
        ircMsgLBName   :: !String,
        ircMsgPrefix   :: !String,
        ircMsgCommand  :: !String,
        ircMsgParams   :: ![String]
  }
  deriving (Show)

instance Message IrcMessage where
    nick                = liftM2 Nick ircMsgServer (takeWhile (/= '!') . ircMsgPrefix)
    server              = ircMsgServer
    fullName            = dropWhile (/= '!') . ircMsgPrefix
    channels msg        = 
      let cstr = head $ ircMsgParams msg
        in map (Nick (server msg)) $
           map (\(x:xs) -> if x == ':' then xs else x:xs) (splitOn "," cstr)
               -- solves what seems to be an inconsistency in the parser
    lambdabotName msg   = Nick (server msg) (ircMsgLBName msg)

-- | 'mkMessage' creates a new message from a server, a cmd, and a list of parameters.
mkMessage :: String -- ^ Server
          -> String -- ^ Command
          -> [String] -- ^ Parameters
          -> IrcMessage -- ^ Returns: The created message

mkMessage svr cmd params = IrcMessage 
    { ircMsgServer = svr
    , ircMsgPrefix = ""
    , ircMsgCommand = cmd
    , ircMsgParams = params
    , ircMsgLBName = "urk!<outputmessage>"
    }

joinChannel :: Nick -> IrcMessage
joinChannel loc     = mkMessage (nTag loc)  "JOIN"  [nName loc]

partChannel :: Nick -> IrcMessage
partChannel loc     = mkMessage (nTag loc)  "PART"  [nName loc]

getTopic :: Nick -> IrcMessage
getTopic chan       = mkMessage (nTag chan) "TOPIC" [nName chan]

setTopic :: Nick -> String -> IrcMessage
setTopic chan topic = mkMessage (nTag chan) "TOPIC" [nName chan, ':' : topic]

-- | 'privmsg' creates a private message to the person designated.
privmsg :: Nick -- ^ Who should recieve the message (nick)
        -> String -- ^ What is the message?
        -> IrcMessage -- ^ Constructed message
privmsg who msg = if action then mk [nName who, ':':(chr 0x1):("ACTION " ++ clean_msg ++ ((chr 0x1):[]))]
                            else mk [nName who, ':' : clean_msg]
    where mk = mkMessage (nTag who) "PRIVMSG"
          cleaned_msg = case filter (/= '\CR') msg of
              str@('@':_) -> ' ':str
              str         -> str
          (clean_msg,action) = case cleaned_msg of
              ('/':'m':'e':r) -> (dropWhile isSpace r,True)
              str             -> (str,False)

-- | 'quit' creates a server QUIT message. The input string given is the
--   quit message, given to other parties when leaving the network.
quit :: String -> String -> IrcMessage
quit svr msg = mkMessage svr "QUIT" [':' : msg]

-- | Construct a privmsg from the CTCP TIME notice, to feed up to
-- the @localtime-reply plugin, which then passes the output to
-- the appropriate client.
timeReply :: IrcMessage -> IrcMessage
timeReply msg = msg
    { ircMsgCommand = "PRIVMSG"
    , ircMsgParams  = [head (ircMsgParams msg)
                   ,":@localtime-reply " ++ (nName $ nick msg) ++ ":" ++
                      (init $ drop 7 (last (ircMsgParams msg))) ]
    }

-- Only needed for Base.hs
errShowMsg :: IrcMessage -> String
errShowMsg msg = "ERROR> <" ++ ircMsgServer msg ++ (':' : ircMsgPrefix msg) ++
      "> [" ++ ircMsgCommand msg ++ "] " ++ show (ircMsgParams msg)

user :: String -> String -> String -> String -> IrcMessage
user svr nick_ server_ ircname = mkMessage svr "USER" [nick_, "localhost", server_, ircname]

setNick :: Nick -> IrcMessage
setNick nick_ = mkMessage (nTag nick_) "NICK" [nName nick_]

