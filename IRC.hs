--
-- | The IRC module processes the IRC protocol and provides a nice API for sending
--   and recieving IRC messages with an IRC server.
--
module IRC where

import Data.Char (chr,isSpace)
import DeepSeq
import Util (split, breakOnGlue, clean)
import qualified Util (concatWith)

-- | An IRC message is a prefix, a command and a list of parameters.
--   They can be DeepSeq'ed.
data Message
  = Message {
        msgPrefix   :: String,
        msgCommand  :: String,
        msgParams   :: [String]
  }
  deriving (Show)

instance DeepSeq Message where
  deepSeq m
    = deepSeq (msgPrefix m) . deepSeq (msgCommand m) . deepSeq (msgParams m)

-- | 'mkMessage' creates a new message from a cmd and a list of parameters.
mkMessage :: String -- ^ Command
          -> [String] -- ^ Parameters
          -> Message -- ^ Returns: The created message
mkMessage cmd params
  = Message { msgPrefix = "", msgCommand = cmd, msgParams = params }

-- | 'nick' extracts the nickname involved in a given message.
nick :: Message -> String
nick = fst . breakOnGlue "!" . msgPrefix

-- | 'fullName' extracts the full user name involved in a given message.
fullName :: Message -> String
fullName = snd . breakOnGlue "!" . msgPrefix

-- | 'channels' extracts the channels a Message operate on.
channels :: Message -> [String]
channels msg
  = let cstr = head $ msgParams msg
    in map (\(x:xs) -> if x == ':' then xs else x:xs) (split "," cstr)
           -- solves what seems to be an inconsistency in the parser

-- | 'privmsg' creates a private message to the person designated.
privmsg :: String -- ^ Who should recieve the message (nick)
        -> String -- ^ What is the message?
        -> Message -- ^ Constructed message
privmsg who msg = if action then mkMessage "PRIVMSG" [who, ':':(chr 0x1):("ACTION " ++ clean_msg ++ ((chr 0x1):[]))]
                            else mkMessage "PRIVMSG" [who, ':' : clean_msg]
    where cleaned_msg = case concatMap clean msg of
              str@('@':_) -> ' ':str
              str         -> str
          (clean_msg,action) = case cleaned_msg of
              ('/':'m':'e':r) -> (dropWhile isSpace r,True)
              str             -> (str,False)

-- | 'setTopic' takes a channel and a topic. It then returns the message
--   which sets the channels topic.
setTopic :: String -- ^ Channel
         -> String -- ^ Topic
         -> Message
setTopic chan topic = mkMessage "TOPIC" [chan, ':' : topic]

-- | 'getTopic' Returns the topic for a channel, given as a String
getTopic :: String -> Message
getTopic chan = mkMessage "TOPIC" [chan]

-- | 'quit' creates a server QUIT message. The input string given is the
--   quitmessage, given to other parties when leaving the network.
quit :: String -> Message
quit msg = mkMessage "QUIT" [':' : msg]

-- | 'join' creates a join message. String given is the location (channel)
--   to join.
join :: String -> Message
join loc = mkMessage "JOIN" [loc]

-- | 'part' parts the channel given.
part :: String -> Message
part loc = mkMessage "PART" [loc]

-- | 'names' builds a NAMES message from a list of channels.
names :: [String] -> Message
names chans = mkMessage "NAMES" [Util.concatWith "," chans]

----------------------------------------------------------------------
-- Encoding and decoding of messages

-- | 'encodeMessage' takes a message and converts it to a function.
--   giving this function a string will attach the string to the message
--   and output a string containing IRC protocol commands ready for writing
--   on the outgoing stream socket.
encodeMessage :: Message -> String -> String
encodeMessage msg
  = encodePrefix (msgPrefix msg) . encodeCommand (msgCommand msg)
          . encodeParams (msgParams msg)
  where
    encodePrefix [] = id
    encodePrefix prefix = showChar ':' . showString prefix . showChar ' '

    encodeCommand cmd = showString cmd

    encodeParams [] = id
    encodeParams (p:ps) = showChar ' ' . showString p . encodeParams ps

-- | 'decodeMessage' Takes an input line from the IRC protocol stream
--   and decodes it into a message.
decodeMessage :: String -> Message
decodeMessage line =
    let (prefix, rest1) = decodePrefix (,) line
        (cmd, rest2)    = decodeCmd (,) rest1
        params          = decodeParams rest2
    in Message { msgPrefix = prefix, msgCommand = cmd, msgParams = params }
  where
    decodePrefix k (':':cs) = decodePrefix' k cs
      where decodePrefix' j ""       = j "" ""
            decodePrefix' j (' ':ds) = j "" ds
            decodePrefix' j (c:ds)   = decodePrefix' (j . (c:)) ds

    decodePrefix k cs = k "" cs

    decodeCmd k []       = k "" ""
    decodeCmd k (' ':cs) = k "" cs
    decodeCmd k (c:cs)   = decodeCmd (k . (c:)) cs

    decodeParams :: String -> [String]
    decodeParams xs = decodeParams' [] [] xs
      where
        decodeParams' param params []
          | null param = reverse params
          | otherwise  = reverse (reverse param : params)
        decodeParams' param params (' ' : cs)
          | null param = decodeParams' [] params cs
          | otherwise  = decodeParams' [] (reverse param : params) cs
        decodeParams' param params rest@(c@':' : cs)
          | null param = reverse (rest : params)
          | otherwise  = decodeParams' (c:param) params cs
        decodeParams' param params (c:cs) = decodeParams' (c:param) params cs
