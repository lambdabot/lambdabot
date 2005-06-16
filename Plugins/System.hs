--
-- | System module : IRC control functions
--
module Plugins.System (theModule) where

import Lambdabot
import LBState
import qualified IRC
import Util                     (breakOnGlue,showClean)
import AltTime
import qualified Map as M       (Map,keys,fromList,lookup,union)

import Data.Maybe               (fromMaybe)
import Data.List                ((\\))
import Control.Monad.State      (MonadState(get), gets)
import Control.Monad.Trans      (liftIO)

------------------------------------------------------------------------

newtype SystemModule = SystemModule ()

theModule :: MODULE
theModule = MODULE $ SystemModule ()

instance Module SystemModule ClockTime where
    moduleCmds   _   = return (M.keys syscmds)
    modulePrivs  _   = return (M.keys privcmds)
    moduleHelp _ s   = return $ fromMaybe defaultHelp 
      (M.lookup s $ syscmds `M.union` privcmds)
    moduleDefState _ = liftIO getClockTime
    process      _   = doSystem

------------------------------------------------------------------------

syscmds :: M.Map String String
syscmds = M.fromList
       [("listchans",   "show channels bot has joined")
       ,("listmodules", "show available plugins")
       ,("listcommands","listcommands [module|command]\n"++
                        "show all commands or command for [module]")
       ,("echo",        "echo irc protocol string")
       ,("uptime",      "show uptime")]

privcmds :: M.Map String String
privcmds = M.fromList [
        ("join",        "join <channel>")
       ,("leave",       "leave <channel>")
       ,("part",        "part <channel>")
       ,("msg",         "msg someone")
       ,("quit",        "quit [msg], have the bot exit with msg")
       ,("reconnect",   "reconnect to channel")]


defaultHelp :: String
defaultHelp = "system : irc management"

doSystem :: IRC.Message -> String -> [Char] -> [Char] -> ModuleT ClockTime LB ()
doSystem msg target cmd rest = do
   s <- get
   case cmd of
      "listchans"   -> ircPrivmsg target $ pprKeys (ircChannels s)
      "listmodules" -> ircPrivmsg target $ pprKeys (ircModules s)
      "listcommands" | null rest -> listAll s target
                     | otherwise -> listModule target rest

      "join"  -> send $ IRC.join rest
      "leave" -> send $ IRC.part rest
      "part"  -> send $ IRC.part rest

      "msg"   -> ircPrivmsg tgt txt'
                      where (tgt, txt) = breakOnGlue " " rest
                            txt'       = dropWhile (== ' ') txt

      "quit" -> ircQuit $ if null rest then "requested" else rest

      "reconnect" -> ircReconnect $ if null rest then "request" else rest

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

listAll :: IRCRWState -> String -> LB ()
listAll state target = do
        privs <- gets ircPrivCommands
        ircPrivmsg target $ showClean (M.keys (ircCommands state) \\ privs)

listModule :: String -> String -> LB ()
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
        privs <- gets ircPrivCommands
        ircPrivmsg target $ concat [?name, " provides: ", showClean $ cmds\\privs]

pprKeys :: (Show k) => M.Map k a -> String
pprKeys m = showClean (M.keys m)
