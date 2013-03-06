-- | The plugin-level IRC interface.

module Lambdabot.Plugin.IRC (theModule) where

import Lambdabot.Plugin
import Lambdabot.Monad
import Lambdabot.IRC

import Control.Concurrent
import qualified Control.Concurrent.SSem as SSem
import qualified Control.Exception as E (catch)
import Control.Monad.Error
import qualified Data.ByteString.Char8 as P
import Data.List
import Data.List.Split
import Network( connectTo, PortID(..) )
import System.IO

type IRC = ModuleT () LB

theModule :: Module ()
theModule = newModule
    { moduleCmds = return
        [ (command "irc-connect")
            { privileged = True
            , help = say "irc-connect tag host portnum nickname userinfo.  connect to an irc server"
            , process = \rest ->
                case splitOn " " rest of
                    tag:hostn:portn:nickn:uix -> do
                        pn <- (PortNumber . fromInteger) `fmap` readM portn
                        lift (online tag hostn pn nickn (intercalate " " uix))
                    _ -> say "Not enough parameters!"
            }
        ]
    }

----------------------------------------------------------------------
-- Encoding and decoding of messages

-- | 'encodeMessage' takes a message and converts it to a function.
--   giving this function a string will attach the string to the message
--   and output a string containing IRC protocol commands ready for writing
--   on the outgoing stream socket.
encodeMessage :: IrcMessage -> String -> String
encodeMessage msg
  = encodePrefix (ircMsgPrefix msg) . encodeCommand (ircMsgCommand msg)
          . encodeParams (ircMsgParams msg)
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
    in IrcMessage { ircMsgServer = svr, ircMsgLBName = lbn, ircMsgPrefix = prefix,
                    ircMsgCommand = cmd, ircMsgParams = params }
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

online :: String -> String -> PortID -> String -> String -> IRC ()
online tag hostn portnum nickn ui = do
  sock <- io $ connectTo hostn portnum
  io $ hSetBuffering sock NoBuffering
  -- Implements flood control: RFC 2813, section 5.8
  sem1 <- io $ SSem.new 0
  sem2 <- io $ SSem.new 4 -- one extra token stays in the MVar
  sendmv <- io $ newEmptyMVar
  _ <- io $ forkIO $ sequence_ $ repeat $ do
    SSem.wait sem1
    threadDelay 2000000
    SSem.signal sem2
  _ <- io $ forkIO $ sequence_ $ repeat $ do
    SSem.wait sem2
    putMVar sendmv ()
    SSem.signal sem1
  catchError (addServer tag $ io . sendMsg tag sock sendmv)
             (\err -> io (hClose sock) >> throwError err)
  lift $ ircSignOn hostn (Nick tag nickn) ui
  _ <- lift $ liftLB forkIO $ catchError (readerLoop tag nickn sock)
                                   (\e -> do io $ hPutStrLn stderr $ "irc[" ++ tag ++ "] error: " ++ show e
                                             remServer tag)
  return ()

readerLoop :: String -> String -> Handle -> LB ()
readerLoop tag nickn sock = do
  line <- io $ hGetLine sock
  let line' = filter (\c -> c /= '\n' && c /= '\r') line
  if "PING " `isPrefixOf` line'
      then io $ hPutStr sock ("PONG " ++ drop 5 line' ++ "\r\n")
      else do _ <- forkLB $ received (decodeMessage tag nickn line')
              return ()
  readerLoop tag nickn sock

sendMsg :: String -> Handle -> MVar () -> IrcMessage -> IO ()
sendMsg tag sock mv msg =
    E.catch (do takeMVar mv
                P.hPut sock $ P.pack $ encodeMessage msg "\r\n")
            (\err -> do hPutStrLn stderr $ "irc[" ++ tag ++ "] error: " ++ show (err :: IOError)
                        hClose sock)
