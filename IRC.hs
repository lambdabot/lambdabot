--
-- | The IRC module processes the IRC protocol and provides a nice API for sending
--   and recieving IRC messages with an IRC server.
--
module IRC ( IrcMessage
           , readerLoop
           , writerLoop
           , offlineReaderLoop
           , offlineWriterLoop
           , privmsg
           , quit
           , timeReply
           , errShowMsg -- TODO: remove
           , user
           , setNick
           ) where

import Message
import Lib.Util (split, breakOnGlue, clean)
import qualified Lib.Util as Util (concatWith) 

import Data.List (isPrefixOf)
import Data.Char (chr,isSpace)

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad (when)

import System.IO (Handle, hGetLine)
import System.Console.Readline  (readline, addHistory)

import qualified Data.ByteString.Char8 as P (pack, hPut)


-- | An IRC message is a prefix, a command and a list of parameters.
data IrcMessage
  = IrcMessage {
        msgPrefix   :: !String,
        msgCommand  :: !String,
        msgParams   :: ![String]
  }
  deriving (Show)

instance Message IrcMessage where
  nick = IRC.nick
  fullName = IRC.fullName
  names = IRC.names
  channels = IRC.channels
  joinChannel = IRC.join
  partChannel = IRC.part
  getTopic = IRC.getTopic
  setTopic = IRC.setTopic
  body = IRC.msgParams
  command = IRC.msgCommand

-- | 'mkMessage' creates a new message from a cmd and a list of parameters.
mkMessage :: String -- ^ Command
          -> [String] -- ^ Parameters
          -> IrcMessage -- ^ Returns: The created message

mkMessage cmd params = IrcMessage { msgPrefix = "", msgCommand = cmd, msgParams = params }

-- | 'nick' extracts the nickname involved in a given message.
nick :: IrcMessage -> String
nick = fst . breakOnGlue "!" . msgPrefix

-- | 'fullName' extracts the full user name involved in a given message.
fullName :: IrcMessage -> String
fullName = snd . breakOnGlue "!" . msgPrefix

-- | 'channels' extracts the channels a IrcMessage operate on.
channels :: IrcMessage -> [String]
channels msg
  = let cstr = head $ msgParams msg
    in map (\(x:xs) -> if x == ':' then xs else x:xs) (split "," cstr)
           -- solves what seems to be an inconsistency in the parser

-- | 'privmsg' creates a private message to the person designated.
privmsg :: String -- ^ Who should recieve the message (nick)
        -> String -- ^ What is the message?
        -> IrcMessage -- ^ Constructed message
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
         -> IrcMessage
setTopic chan topic = mkMessage "TOPIC" [chan, ':' : topic]

-- | 'getTopic' Returns the topic for a channel, given as a String
getTopic :: String -> IrcMessage
getTopic chan = mkMessage "TOPIC" [chan]

-- | 'quit' creates a server QUIT message. The input string given is the
--   quit message, given to other parties when leaving the network.
quit :: String -> IrcMessage
quit msg = mkMessage "QUIT" [':' : msg]

-- | 'join' creates a join message. String given is the location (channel)
--   to join.
join :: String -> IrcMessage
join loc = mkMessage "JOIN" [loc]

-- | 'part' parts the channel given.
part :: String -> IrcMessage
part loc = mkMessage "PART" [loc]

-- | 'names' builds a NAMES message from a list of channels.
names :: [String] -> IrcMessage
names chans = mkMessage "NAMES" [Util.concatWith "," chans]

-- | Construct a privmsg from the CTCP TIME notice, to feed up to
-- the @localtime-reply plugin, which then passes the output to
-- the appropriate client.
timeReply :: IrcMessage -> IrcMessage
timeReply msg    = 
   IrcMessage { msgPrefix  = msgPrefix (msg)
              , msgCommand = "PRIVMSG"
              , msgParams  = [head (msgParams msg)
                             ,":@localtime-reply " ++ (IRC.nick msg) ++ ":" ++
                                (init $ drop 7 (last (msgParams msg))) ]
              }

-- Only needed for Base.hs
errShowMsg :: IrcMessage -> String
errShowMsg msg = "ERROR> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)

user :: String -> String -> String -> IrcMessage
user nick_ server ircname = IRC.mkMessage "USER" [nick_, "localhost", server, ircname]

setNick :: String -> IrcMessage
setNick nick_ = IRC.mkMessage "NICK" [nick_]
----------------------------------------------------------------------
-- Encoding and decoding of messages

-- | 'encodeMessage' takes a message and converts it to a function.
--   giving this function a string will attach the string to the message
--   and output a string containing IRC protocol commands ready for writing
--   on the outgoing stream socket.
encodeMessage :: IrcMessage -> String -> String
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
decodeMessage :: String -> IrcMessage
decodeMessage line =
    let (prefix, rest1) = decodePrefix (,) line
        (cmd, rest2)    = decodeCmd (,) rest1
        params          = decodeParams rest2
    in IrcMessage { msgPrefix = prefix, msgCommand = cmd, msgParams = params }
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

------------------------------------------------------------------------
--
-- Lambdabot is asynchronous. We has reader and writer threads, and they
-- don't know about each other.
--
-- However, in Offline mode, we need to keep them in lock step. this
-- complicates things.
--
-- Online reader loop, the mvars are unused

readerLoop :: ThreadId -> Pipe IrcMessage -> Pipe IrcMessage -> Handle -> MVar () -> MVar () -> IO ()
readerLoop th chanr chanw h _ _ = handleIO th $ do
    io (putStrLn "Running reader loop...")
    readerLoop'
  where
    readerLoop' = do
        line <- hGetLine h
        let line' = filter (\c -> c /= '\n' && c /= '\r') line
        if pING `isPrefixOf` line'
            then writeChan chanw (Just $ IRC.mkMessage "PONG" [drop 5 line'])
            else writeChan chanr (Just $ IRC.decodeMessage line')
        readerLoop'

    pING = "PING "
{-# INLINE readerLoop #-}

--
-- online writer loop
--
-- Implements flood control: RFC 2813, section 5.8
--
writerLoop :: ThreadId -> Pipe IrcMessage -> Handle -> MVar () -> MVar () -> IO ()
writerLoop th chanw h _ _ = handleIO th $ do
    sem1 <- newQSem 0
    sem2 <- newQSem 5
    forkIO $ sequence_ . repeat $ do
           waitQSem sem1
           threadDelay 2000000
           signalQSem sem2
    writerLoop' (sem1,sem2)
  where
    writerLoop' sems@(sem1,sem2) = do
           mmsg <- readChan chanw
           waitQSem sem2
           case mmsg of
            Nothing  -> return ()
            Just msg -> P.hPut h $ P.pack $ IRC.encodeMessage msg "\r"
           signalQSem sem1
           writerLoop' sems
{-# INLINE writerLoop #-}


-- 
-- Offline reader and writer loops. A prompt with line editing
-- Takes a string from stdin, wraps it as an irc message, and _blocks_
-- waiting for the writer thread (to keep things in sync).
--
-- We (incorrectly) assume there's at least one write for every read.
-- If a command returns no output (i.e. @more on an empty buffer) then
-- we block in offline mode :(
-- 
-- the mvars are used to keep the normally async threads in step.
--
offlineReaderLoop :: ThreadId -> Pipe IrcMessage -> Pipe IrcMessage -> Handle
                  -> MVar () -> MVar () -> IO ()
offlineReaderLoop th chanr _chanw _h syncR syncW = handleIO th readerLoop'
  where
    readerLoop' = do
        takeMVar syncR  -- wait till writer lets us proceed
        s <- readline "lambdabot> " -- read stdin
        case s of
            Nothing -> error "<eof>"
            Just x -> let s' = dropWhile isSpace x
                      in if null s' then putMVar syncR () >> readerLoop' else do
                addHistory s'

                let mmsg = case s' of
                            "quit" -> Nothing
                            '>':xs -> Just $ "@run " ++ xs
                            _      -> Just $ "@"     ++ dropWhile (== ' ') s'

                msg <- case mmsg of
                    Nothing   -> error "<quit>"
                    Just msg' -> return msg'

                let m  = IRC.IrcMessage { IRC.msgPrefix  = "null!n=user@null"
                                        , IRC.msgCommand = "PRIVMSG"
                                        , IRC.msgParams  = ["#haskell",":" ++ msg ] }
                writeChan chanr (Just m)
                putMVar syncW () -- let writer go 
                readerLoop'

--
-- Offline writer. Print to stdout
--
offlineWriterLoop :: ThreadId -> Pipe IrcMessage -> Handle -> MVar () -> MVar () -> IO ()
offlineWriterLoop th chanw h syncR syncW = handleIO th writerLoop'
  where
    writerLoop' = do

        takeMVar syncW -- wait for reader to let us go

        let loop = do
            mmsg <- readChan chanw
            case mmsg of
                Nothing  -> return ()
                Just msg -> do
                    let str = case (tail . IRC.msgParams) msg of
                                []    -> []
                                (x:_) -> tail x
                    P.hPut h (P.pack $ str ++ "\n")     -- write stdout
            threadDelay 25 -- just for fun.
            b <- isEmptyChan chanw
            when (not b) loop
        loop

        putMVar syncR () -- now allow writer to go
        writerLoop'

-- convenience:
io :: forall a (m :: * -> *). (MonadIO m) => IO a -> m a
io = liftIO
{-# INLINE io #-}

-- Thread handler, just catch particular things we want to throw out to
-- the main thread, to force an exit. errorCalls are used by the
-- reader/writer loops to exit. ioErrors are probably sockets closing.
handleIO :: ThreadId -> IO () -> IO ()
handleIO th = handleJust
    (\e -> case () of { _
                | Just _ <- errorCalls e -> Just e
                | Just _ <- ioErrors   e -> Just e
                | otherwise              -> Nothing
    }) (\e -> throwTo th (error (show e)))

