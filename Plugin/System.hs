--
-- | System module : IRC control functions
--
module Plugin.System (theModule) where

import Plugin
import Lib.AltTime
import qualified IRC
import qualified Data.Map as M       (Map,keys,fromList,lookup,union)

import Control.Monad.State      (MonadState(get), gets)

------------------------------------------------------------------------

newtype SystemModule = SystemModule ()

theModule :: MODULE
theModule = MODULE $ SystemModule ()

instance Module SystemModule ClockTime where
    moduleCmds   _   = M.keys syscmds
    modulePrivs  _   = M.keys privcmds
    moduleHelp _ s   = fromMaybe defaultHelp (M.lookup s $ syscmds `M.union` privcmds)
    moduleDefState _ = io getClockTime
    process      _   = doSystem

------------------------------------------------------------------------

syscmds :: M.Map String String
syscmds = M.fromList
       [("listchans",   "Show channels bot has joined")
       ,("listmodules", "listmodules. Show available plugins")
       ,("list",        "list [module|command]\n"++
                        "show all commands or command for [module]")
       ,("echo",        "echo <msg>. echo irc protocol string")
       ,("uptime",      "uptime. Show uptime")]

privcmds :: M.Map String String
privcmds = M.fromList [
        ("join",        "join <channel>")
       ,("leave",       "leave <channel>")
       ,("part",        "part <channel>")
       ,("msg",         "msg <nick or channel> <msg>")
       ,("quit",        "quit [msg], have the bot exit with msg")
       ,("reconnect",   "reconnect to server")]

------------------------------------------------------------------------

defaultHelp :: String
defaultHelp = "system : irc management"

doSystem :: IRC.Message -> String -> [Char] -> [Char] -> ModuleLB ClockTime
doSystem msg target cmd rest = get >>= \s -> case cmd of

  "listchans"   -> return [pprKeys (ircChannels s)]
  "listmodules" -> return [pprKeys (ircModules s) ]
  "list" 
        | null rest -> case target of
              ('#':_) -> return ["list [module|command]. " ++ 
                                 "Where modules is one of:\n" ++ pprKeys (ircModules s)]
              _       -> listAll
        | otherwise -> listModule rest >>= return . (:[])

  ------------------------------------------------------------------------

  "join"  -> send (IRC.join rest) >> return []        -- system commands
  "leave" -> send (IRC.part rest) >> return []     
  "part"  -> send (IRC.part rest) >> return []     

   -- writes to another location:
  "msg"   -> ircPrivmsg tgt txt' >> return []
                  where (tgt, txt) = breakOnGlue " " rest
                        txt'       = dropWhile (== ' ') txt

  "quit" -> do ircQuit $ if null rest then "requested" else rest
               return []

  "reconnect" -> do ircReconnect $ if null rest then "request" else rest
                    return []

  "echo" -> return [concat ["echo; msg:", show msg, " rest:", show rest]]

  "uptime" -> do
          loaded <- readMS
          now    <- io getClockTime
          let diff = timeDiffPretty $ now `diffClockTimes` loaded
          return ["uptime: " ++ diff]

------------------------------------------------------------------------

listAll :: LB [String]
listAll = get >>= mapM listModule . M.keys . ircModules

listModule :: String -> LB String
listModule query = withModule ircModules query fromCommand printProvides
  where
    fromCommand = withModule ircCommands query
        (return $ "No module \""++query++"\" loaded")
        printProvides
    
    printProvides m = do
        let cmds = moduleCmds m
        privs <- gets ircPrivCommands
        let cmds' = cmds \\ privs -- don't display privledged commands
        return . concat $ if null cmds' 
                            then [?name, " has no visible commands"]
                            else [?name, " provides: ", showClean $ cmds\\privs]

pprKeys :: (Show k) => M.Map k a -> String
pprKeys m = showClean (M.keys m)
