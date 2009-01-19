{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | The plugin-level IRC interface.

module Plugin.IRC (theModule) where

import Control.Concurrent( forkIO, newQSem, waitQSem, threadDelay, signalQSem,
                           newEmptyMVar, putMVar, takeMVar, MVar )
import Control.OldException
import IRCBase
import LMain( received )
import Message
import Network( connectTo, PortID(..) )
import Plugin
import System.IO (hGetLine, hPutStr, hPutStrLn, hSetBuffering, BufferMode(NoBuffering), stderr, hClose)
import qualified Data.ByteString.Char8 as P

$(plugin "IRC")

instance Module IRCModule () where
    modulePrivs  _         = ["irc-connect"]
    moduleHelp _ _         = "irc-connect tag host portnum nickname userinfo.  connect to an irc server"
    process_ _ "irc-connect" rest =
        case (split " " rest) of
          tag:hostn:portn:nickn:uix -> do pn <- (PortNumber . fromInteger) `fmap` readM portn
                                          online tag hostn pn nickn (concatWith " " uix)
                                          return []
          _                         -> return ["Not enough parameters!"]

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
--   and decodes it into a message.  FIXME: this has too many parameters.
decodeMessage :: String -> String -> String -> IrcMessage
decodeMessage svr lbn line =
    let (prefix, rest1) = decodePrefix (,) line
        (cmd, rest2)    = decodeCmd (,) rest1
        params          = decodeParams rest2
    in IrcMessage { msgServer = svr, msgLBName = lbn, msgPrefix = prefix,
                    msgCommand = cmd, msgParams = params }
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
    send $ user (nTag nickn) (nName nickn) svr ircname
    send $ setNick nickn

------------------------------------------------------------------------
--
-- Lambdabot is mostly synchronous.  We have a main loop, which reads
-- messages and forks threads to execute commands (which write responces).
-- OR
-- We have a main loop which reads offline commands, and synchronously
-- interprets them.

online :: String -> String -> PortID -> String -> String -> ModuleT () LB ()
online tag hostn portnum nickn ui = do
  sock <- io $ connectTo hostn portnum
  io $ hSetBuffering sock NoBuffering
  -- Implements flood control: RFC 2813, section 5.8
  sem1 <- io $ newQSem 0
  sem2 <- io $ newQSem 4 -- one extra token stays in the MVar
  sendmv <- io $ newEmptyMVar
  io $ forkIO $ sequence_ $ repeat $ do
    waitQSem sem1
    threadDelay 2000000
    signalQSem sem2
  io $ forkIO $ sequence_ $ repeat $ do
    waitQSem sem2
    putMVar sendmv ()
    signalQSem sem1
  catchError (addServer tag $ io . sendMsg tag sock sendmv)
             (\err -> io (hClose sock) >> throwError err)
  lift $ ircSignOn hostn (Nick tag nickn) ui
  lift $ liftLB forkIO $ catchError (readerLoop tag nickn sock)
                                   (\e -> do io $ hPutStrLn stderr $ "irc[" ++ tag ++ "] error: " ++ show e
                                             remServer tag)
  return ()

readerLoop :: String -> String -> Handle -> LB ()
readerLoop tag nickn sock = do
  line <- io $ hGetLine sock
  let line' = filter (\c -> c /= '\n' && c /= '\r') line
  if "PING " `isPrefixOf` line'
      then io $ hPutStr sock ("PONG " ++ drop 5 line' ++ "\r\n")
      else do forkLB $ received (decodeMessage tag nickn line')
              return ()
  readerLoop tag nickn sock

sendMsg :: String -> Handle -> MVar () -> IrcMessage -> IO ()
sendMsg tag sock mv msg =
    catchJust ioErrors (do takeMVar mv
                           P.hPut sock $ P.pack $ encodeMessage msg "\r\n")
                  (\err -> do hPutStrLn stderr $ "irc[" ++ tag ++ "] error: " ++ show err
                              hClose sock)
