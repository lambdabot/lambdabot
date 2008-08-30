{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}
-- | System module : IRC control functions
module Plugin.System (theModule) where

import Plugin
import Lambdabot.AltTime
import qualified Message (Message, Nick, joinChannel, partChannel, server, readNick)
import qualified Data.Map as M       (Map,keys,fromList,lookup,union,insert,delete)

import Control.Monad.State      (MonadState(get, put), gets)

$(plugin "System")

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
       ,("listservers", "listservers. Show current servers")
       ,("list",        "list [module|command]\n"++
                        "show all commands or command for [module]. http://code.haskell.org/lambdabot/COMMANDS")
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
       ,("admin",       "admin [+|-] nick. change a user's admin status.")
       ,("ignore",      "ignore [+|-] nick. change a user's ignore status.")
       ,("reconnect",   "reconnect to server")]

------------------------------------------------------------------------

defaultHelp :: String
defaultHelp = "system : irc management"

doSystem :: Message.Message a => a -> Message.Nick -> [Char] -> [Char] -> ModuleLB (ClockTime, TimeDiff)
doSystem msg _ cmd rest = get >>= \s -> case cmd of
  "listchans"   -> return [pprKeys (ircChannels s)]
  "listmodules" -> return [pprKeys (ircModules s) ]
  "listservers" -> return [pprKeys (ircServerMap s)]
  "listall"     -> lift listAll
  "list"| null rest -> return ["http://code.haskell.org/lambdabot/COMMANDS"]
        | otherwise -> lift $ listModule rest >>= return . (:[])

  ------------------------------------------------------------------------

  --TODO error handling
   -- system commands
  "join"  -> lift $ send (Message.joinChannel (Message.readNick msg rest)) >> return []
  "leave" -> lift $ send (Message.partChannel (Message.readNick msg rest)) >> return []
  "part"  -> lift $ send (Message.partChannel (Message.readNick msg rest)) >> return []

   -- writes to another location:
  "msg"   -> lift $ ircPrivmsg (Message.readNick msg tgt) txt' >> return []
                  where (tgt, txt) = breakOnGlue " " rest
                        txt'       = dropWhile (== ' ') txt

  "quit" -> lift $ do ircQuit (Message.server msg) $ if null rest then "requested" else rest
                      return []

  "reconnect" -> lift $ do ircReconnect (Message.server msg) $ if null rest then "request" else rest
                           return []

  "echo" -> return [concat ["echo; msg:", show msg, " rest:", show rest]]

  "flush" -> lift $ do flushModuleState
                       return []

  "admin" -> do let pu = ircPrivilegedUsers s
                pu' <- case rest of '+':' ':_ -> return $ M.insert nck True pu
                                    '-':' ':_ -> return $ M.delete nck pu
                                    _         -> fail "@admin: invalid usage"
                put (s {ircPrivilegedUsers = pu'})
                return []
      where nck = Message.readNick msg (drop 2 rest)

  "ignore" -> do let iu = ircIgnoredUsers s
                 iu' <- case rest of '+':' ':_ -> return $ M.insert nck True iu
                                     '-':' ':_ -> return $ M.delete nck iu
                                     _         -> fail "@ignore: invalid usage"
                 put (s {ircIgnoredUsers = iu'})
                 return []
      where nck = Message.readNick msg (drop 2 rest)

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

