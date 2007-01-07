--
-- | The IRC module processes the IRC protocol and provides a nice API for sending
--   and recieving IRC messages with an IRC server.
--
module IRC ( online
           , offline
           ) where

import IRCBase

import Lib.Util (timeout, io)
import Lib.Serial (readM)

import Message

import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Control.Concurrent
import Control.Exception
import Control.Monad.Error

import System.IO (hGetLine, hFlush, hPutStr, hPutStrLn, hSetBuffering, BufferMode(NoBuffering), stdout, stderr, hClose)
import System.Console.Readline  (readline, addHistory)

import qualified Data.ByteString.Char8 as P

import Lambdabot
import LBState

import Network( connectTo, PortID(..) )

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
decodeMessage :: String -> String -> IrcMessage
decodeMessage svr line =
    let (prefix, rest1) = decodePrefix (,) line
        (cmd, rest2)    = decodeCmd (,) rest1
        params          = decodeParams rest2
    in IrcMessage { msgServer = svr, msgPrefix = prefix, msgCommand = cmd, msgParams = params }
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

ircSignOn :: String -> Nick -> String -> LB ()
ircSignOn svr nickn ircname = do
    -- password support.
    -- If plugin initialising was delayed till after we connected, we'd
    -- be able to write a Passwd plugin.
    send $ user (nTag nickn) (nName nickn) svr ircname
    send $ setNick nickn
    mpasswd <- liftIO (handleJust ioErrors (const (return "")) $
                       readFile "State/passwd")
    case readM mpasswd of
      Nothing     -> return ()
      Just passwd -> ircPrivmsg (Nick (nTag nickn) "nickserv") $ "identify " ++ passwd

------------------------------------------------------------------------
--
-- Lambdabot is mostly synchronous.  We have a main loop, which reads
-- messages and forks threads to execute commands (which write responces).
-- OR
-- We have a main loop which reads offline commands, and synchronously
-- interprets them.

online :: String -> String -> PortID -> String -> String -> (IrcMessage -> LB ()) -> LB ()
online tag host portnum nickn ui recv = do
  sock <- io $ connectTo host portnum
  io $ hSetBuffering sock NoBuffering
  -- Implements flood control: RFC 2813, section 5.8
  sem1 <- io $ newQSem 0
  sem2 <- io $ newQSem 5
  io $ forkIO $ sequence_ $ repeat $ do
    waitQSem sem1
    threadDelay 2000000
    signalQSem sem2
  catchError (addServer' tag $ sendMsg sock sem1 sem2)
             (\err -> io (hClose sock) >> throwError err)
  liftLB forkIO $ catchError (do ircSignOn host (Nick tag nickn) ui
                                 readerLoop sock)
                             (\e -> do io $ hPutStrLn stderr $ "irc[" ++ tag ++ "] error: " ++ show e
                                       remServer tag)
  return ()
    where
      readerLoop sock = do
         line <- liftIO $ hGetLine sock
         let line' = filter (\c -> c /= '\n' && c /= '\r') line
         if pING `isPrefixOf` line'
           then liftIO $ hPutStr sock ("PONG " ++ drop 5 line')
           else do forkLB $ recv (IRC.decodeMessage tag line')
                   return ()
         readerLoop sock
      pING = "PING "
      sendMsg sock sem1 sem2 msg =
          catchError (liftIO $ do waitQSem sem2
                                  P.hPut sock $ P.pack $ IRC.encodeMessage msg "\r"
                                  signalQSem sem1)
                     (\err -> do io $ hPutStrLn stderr $ "irc[" ++ tag ++ "] error: " ++ show err
                                 io $ hClose sock)
                 
                               
 
-- 
-- Offline reader and writer loops. A prompt with line editing
-- Takes a string from stdin, wraps it as an irc message, and _blocks_
-- waiting for the writer thread (to keep things in sync).
--
offline :: String -> (IrcMessage -> LB ()) -> LB ()
offline tag recv = do
  addServer' tag sendMsg
  liftLB forkIO $ catchError readerLoop
                             (\e -> do io $ hPutStrLn stderr $ "rl[" ++ tag ++ "] error: " ++ show e
                                       remServer tag)
  return ()
    where
      readerLoop = do
         line <- liftIO $ readline "lambdabot> "
         case line of
           Nothing -> error "<eof>"
           Just x -> let s' = dropWhile isSpace x
                     in if null s' then readerLoop else do
                liftIO $ addHistory s'

                let mmsg = case s' of
                            "quit" -> Nothing
                            '>':xs -> Just $ "@run " ++ xs
                            '!':xs -> Just $ xs     -- trigger contextual stuff
                            _      -> Just $ "@"     ++ dropWhile (== ' ') s'

                msg <- case mmsg of
                    Nothing   -> fail "<quit>"
                    Just msg' -> return msg'

                let m  = IrcMessage { msgPrefix  = "null!n=user@null"
                                    , msgServer  = tag
                                    , msgCommand = "PRIVMSG"
                                    , msgParams  = ["offline",":" ++ msg ] }
                liftLB (timeout (15 * 1000 * 1000)) $ recv m
                readerLoop

--
-- Offline writer. Print to stdout
--
      sendMsg msg = liftIO $ do
                      let str = case (tail . msgParams) msg of
                                  []    -> []
                                  (x:_) -> tail x
                      P.hPutStrLn stdout (P.pack str)
                      hFlush stdout
