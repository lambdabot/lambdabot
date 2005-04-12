--
-- | System module : IRC control functions
--

module SystemModule (theModule) where

import IRC
import Util                     (join, breakOnGlue)
import qualified Map as M       (Map,keys,fromList,lookup)

import Data.Maybe               (fromMaybe)
import Control.Monad.State      (MonadState(get))

------------------------------------------------------------------------

newtype SystemModule = SystemModule ()

systemModule :: SystemModule
systemModule = SystemModule ()

theModule :: MODULE
theModule = MODULE systemModule

instance Module SystemModule () where
    moduleName   _ = "system"
    moduleCmds   _ = return (M.keys syscmds)
    moduleHelp _ s = return $ fromMaybe defaultHelp (M.lookup s syscmds)
    process      _ = doSystem

------------------------------------------------------------------------

syscmds :: M.Map String String
syscmds = M.fromList
       [("listchans",   "show channels bot has joined")
       ,("listmodules", "show available plugins")
       ,("listcommands","listcommands [module]\n"++
                        "show all commands or command for [module]")
       ,("join",        "join <channel>")
       ,("leave",       "leave <channel>")
       ,("part",        "part <channel>")
       ,("msg",         "msg someone")
       ,("quit",        "quit [msg]")
       ,("reconnect",   "reconnect to channel")
       ,("echo",        "echo irc protocol string")]

defaultHelp :: String
defaultHelp = "system : irc management"

doSystem :: IRCMessage -> String -> [Char] -> [Char] -> IRC ()
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

      _unknowncmd -> ircPrivmsg target $ 
              concat ["unknown system command: ", show msg, show rest]

------------------------------------------------------------------------

listAll :: IRCRWState -> String -> IRC ()
listAll state target = 
        ircPrivmsg target $ "Commands: "++pprKeys (ircCommands state)

listModule :: String -> String -> IRC ()
listModule target modname = withModule ircCommands modname (ircPrivmsg target $ 
        "No module \""++modname++"\" loaded") (\m -> do
                cmds <- liftLB $ moduleCmds m
                ircPrivmsg target $ concat 
                        ["Module ", modname, 
                         " provides the following commands: ", show cmds])

pprKeys :: Show a => M.Map a b -> String
pprKeys = join " " . map (init . tail . show) . M.keys
