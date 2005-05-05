--
-- | System module : IRC control functions
--
module Plugins.System (theModule) where

import Lambdabot
import Util                     (breakOnGlue,showClean)
import AltTime
import qualified Map as M       (Map,keys,fromList,lookup)

import Data.Maybe               (fromMaybe)
import Control.Monad.State      (MonadState(get))
import Control.Monad.Trans      (liftIO)

------------------------------------------------------------------------

newtype SystemModule = SystemModule ()

theModule :: MODULE
theModule = MODULE $ SystemModule ()

instance Module SystemModule ClockTime where
    moduleCmds   _   = return (M.keys syscmds)
    moduleHelp _ s   = return $ fromMaybe defaultHelp (M.lookup s syscmds)
    moduleDefState _ = liftIO getClockTime
    process      _   = doSystem

------------------------------------------------------------------------

syscmds :: M.Map String String
syscmds = M.fromList
       [("listchans",   "show channels bot has joined")
       ,("listmodules", "show available plugins")
       ,("listcommands","listcommands [module|command]\n"++
                        "show all commands or command for [module]")
       ,("join",        "join <channel>")
       ,("leave",       "leave <channel>")
       ,("part",        "part <channel>")
       ,("msg",         "msg someone")
       ,("quit",        "quit [msg], have the bot exit with msg")
       ,("reconnect",   "reconnect to channel")
       ,("echo",        "echo irc protocol string")
       ,("uptime",      "show uptime")]

defaultHelp :: String
defaultHelp = "system : irc management"

doSystem :: IRCMessage -> String -> [Char] -> [Char] -> ModuleT ClockTime IRC ()
doSystem msg target cmd rest = do
   s <- get
   case cmd of
      "listchans"   -> ircPrivmsg target $ pprKeys (ircChannels s)
      "listmodules" -> ircPrivmsg target $ pprKeys (ircModules s)
      "listcommands" | null rest -> listAll s target
                     | otherwise -> listModule target rest

      "join"  -> checkPrivs msg target (ircJoin rest)
      "leave" -> checkPrivs msg target (ircPart rest)
      "part"  -> checkPrivs msg target (ircPart rest)

      "msg"   -> checkPrivs msg target $ ircPrivmsg tgt txt'
                      where (tgt, txt) = breakOnGlue " " rest
                            txt'       = dropWhile (== ' ') txt

      "quit" -> checkPrivs msg target $
              ircQuit $ if null rest then "requested" else rest

      "reconnect" -> checkPrivs msg target $
              ircReconnect $ if null rest then "request" else rest

      "echo" -> ircPrivmsg target $ concat 
              ["echo; msg:", show msg, " rest:", show rest]

      "uptime" -> do
              loaded <- readMS
              now    <- liftIO getClockTime
              let diff = timeDiffPretty $ now `diffClockTimes` loaded
              ircPrivmsg target $ "uptime: " ++ diff

      _unknowncmd -> ircPrivmsg target $ 
              concat ["unknown system command: ", show msg, show rest]

------------------------------------------------------------------------

listAll :: IRCRWState -> String -> IRC ()
listAll state target = 
        ircPrivmsg target $ pprKeys (ircCommands state)

listModule :: String -> String -> IRC ()
listModule target query = withModule ircModules query fromCommand printProvides
    where
    fromCommand = withModule ircCommands query 
        (ircPrivmsg target $ "No module \""++query++"\" loaded") 
        printProvides
    
    -- calling moduleCmds seems bad here, since it might have side effects.
    -- Two solutions come to mind:
    -- (i)  make moduleCmds a pure function (its implementation in all
    --      modules is pure anyway.
    -- (ii) extract the information directly from the ircCommands map.
    printProvides m = do
        cmds <- moduleCmds m
        ircPrivmsg target $ concat [?name, " provides: ", showClean cmds]

pprKeys :: (Show k) => M.Map k a -> String
pprKeys m = showClean (M.keys m)
