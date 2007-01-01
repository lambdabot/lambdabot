--
-- | System module : IRC control functions
--
module Plugin.System (theModule) where

import Plugin
import Lib.AltTime
import qualified Message (Message, joinChannel, partChannel)
import qualified Data.Map as M       (Map,keys,fromList,lookup,union)

import Control.Monad.State      (MonadState(get), gets)

PLUGIN System

instance Module SystemModule (ClockTime, TimeDiff) where
    moduleCmds   _   = M.keys syscmds
    modulePrivs  _   = M.keys privcmds
    moduleHelp _ s   = fromMaybe defaultHelp (M.lookup s $ syscmds `M.union` privcmds)
    process      _   = doSystem
    moduleDefState _ = flip (,) noTimeDiff `fmap` io getClockTime
    moduleSerialize  = const $ Just stdSerial
    moduleInit _     = do (_, d) <- readMS
                          t      <- liftIO getClockTime
                          writeMS (t, d)
    moduleExit _     = do (initial, d) <- readMS
                          now          <- liftIO getClockTime
                          writeMS (initial, max d (diffClockTimes now initial))

------------------------------------------------------------------------

syscmds :: M.Map String String
syscmds = M.fromList
       [("listchans",   "Show channels bot has joined")
       ,("listmodules", "listmodules. Show available plugins")
       ,("list",        "list [module|command]\n"++
                        "show all commands or command for [module]. http://www.cse.unsw.edu.au/~dons/lambdabot/COMMANDS")
       ,("echo",        "echo <msg>. echo irc protocol string")
       ,("uptime",      "uptime. Show uptime")]

privcmds :: M.Map String String
privcmds = M.fromList [
        ("join",        "join <channel>")
       ,("leave",       "leave <channel>")
       ,("part",        "part <channel>")
       ,("msg",         "msg <nick or channel> <msg>")
       ,("quit",        "quit [msg], have the bot exit with msg")
       ,("listall",     "list all commands")
       ,("flush",       "flush. flush state to disk")
       ,("reconnect",   "reconnect to server")]

------------------------------------------------------------------------

defaultHelp :: String
defaultHelp = "system : irc management"

doSystem :: Message.Message a => a -> String -> [Char] -> [Char] -> ModuleLB (ClockTime, TimeDiff)
doSystem msg _ cmd rest = get >>= \s -> case cmd of
  "listchans"   -> return [pprKeys (ircChannels s)]
  "listmodules" -> return [pprKeys (ircModules s) ]
  "listall"     -> lift listAll
  "list"| null rest -> return ["http://www.cse.unsw.edu.au/~dons/lambdabot/COMMANDS"]
        | otherwise -> lift $ listModule rest >>= return . (:[])

  ------------------------------------------------------------------------

  --TODO error handling
   -- system commands
  "join"  -> lift $ send_ (Message.joinChannel rest) >> return []
  "leave" -> lift $ send_ (Message.partChannel rest) >> return []
  "part"  -> lift $ send_ (Message.partChannel rest) >> return []

   -- writes to another location:
  "msg"   -> lift $ ircPrivmsg tgt (Just txt') >> return []
                  where (tgt, txt) = breakOnGlue " " rest
                        txt'       = dropWhile (== ' ') txt

  "quit" -> lift $ do ircQuit $ if null rest then "requested" else rest
                      return []

  "reconnect" -> lift $ do ircReconnect $ if null rest then "request" else rest
                           return []

  "echo" -> return [concat ["echo; msg:", show msg, " rest:", show rest]]

  "flush" -> lift $ do flushModuleState
                       return []

  "uptime" -> do
          (loaded, m) <- readMS
          now         <- io getClockTime
          let diff = now `diffClockTimes` loaded
          return ["uptime: "           ++ timeDiffPretty diff ++
                  ", longest uptime: " ++ timeDiffPretty (max diff m)]

------------------------------------------------------------------------

listAll :: LB [String]
listAll = get >>= mapM listModule . M.keys . ircModules

listModule :: String -> LB String
listModule s = withModule ircModules s fromCommand printProvides
  where
    fromCommand = withModule ircCommands s
        (return $ "No module \""++s++"\" loaded") printProvides

    -- ghc now needs a type annotation here
    printProvides :: (forall mod s. Module mod s => mod -> ModuleT s LB String)
    printProvides m = do
        let cmds = moduleCmds m
        privs <- gets ircPrivCommands
        let cmds' = cmds \\ privs -- don't display privledged commands
        name' <- getName
        return . concat $ if null cmds'
                          then [name', " has no visible commands"]
                          else [name', " provides: ", showClean cmds']

