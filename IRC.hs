--
-- | The IRC module processes the IRC protocol and provides a nice API for sending
--   and recieving IRC messages with an IRC server.
--
module IRC ( online
           , offline
           ) where

import IRCBase

import Config (config, name, port, host, protocol, userinfo )

import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans ( MonadIO, liftIO )

import System.IO (hGetLine, hFlush, hPutStr, hSetBuffering, BufferMode(NoBuffering), stdout)
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

------------------------------------------------------------------------
--
-- Lambdabot is mostly synchronous.  We have a main loop, which reads
-- messages and forks threads to execute commands (which write responces).
-- OR
-- We have a main loop which reads offline commands, and synchronously
-- interprets them.

online :: String -> (IrcMessage -> LB ()) -> IO (LB (), IrcMessage -> LB ())
online tag recv = do
  th <- myThreadId
  let portnum  = PortNumber $ fromIntegral (Config.port Config.config)
  sock <- connectTo (Config.host Config.config) portnum
  hSetBuffering sock NoBuffering
  -- Implements flood control: RFC 2813, section 5.8
  sem1 <- newQSem 0
  sem2 <- newQSem 5
  forkIO $ sequence_ . repeat $ do
                    waitQSem sem1
                    threadDelay 2000000
                    signalQSem sem2
  let h = liftLB (handleIO th)
  return (serverSignOn (protocol config) (name config) (userinfo config) >> readerLoop sock,
          \m -> h (sendMsg sock sem1 sem2 m))
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
      sendMsg sock sem1 sem2 msg = liftIO $ do
          waitQSem sem2
          P.hPut sock $ P.pack $ IRC.encodeMessage msg "\r"
          signalQSem sem1
 
-- 
-- Offline reader and writer loops. A prompt with line editing
-- Takes a string from stdin, wraps it as an irc message, and _blocks_
-- waiting for the writer thread (to keep things in sync).
--
offline :: String -> (IrcMessage -> LB ()) -> IO (LB (), IrcMessage -> LB ())
offline tag recv = do
  th <- myThreadId
  let h = liftLB (handleIO th)
  return (readerLoop, \m -> h (sendMsg m))
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
                recv m
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

