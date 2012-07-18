-- | System module : IRC control functions
module Plugin.System (theModule) where

import Plugin
import Lambdabot

import Lambdabot.AltTime
import qualified Lambdabot.Message as Msg (Message, Nick, joinChannel, partChannel, server, readNick)
import qualified Data.Map as M       (Map,assocs,keys,fromList,insert,delete)

import Control.Monad.State      (MonadState(get, put))

type SystemState = (ClockTime, TimeDiff)
type System = ModuleT SystemState LB

theModule = newModule
    { moduleCmds = return $
        [ (command name)
            { help = say helpStr
            , process = doSystem name
            }
        | (name, helpStr) <- M.assocs syscmds
        ] ++
        [ (command name) 
            { privileged = True
            , help = say helpStr
            , process = doSystem name
            }
        | (name, helpStr) <- M.assocs privcmds
        ]
    , moduleDefState = flip (,) noTimeDiff `fmap` io getClockTime
    , moduleSerialize  = Just stdSerial
    
    , moduleInit = do
        (_, d) <- readMS
        t      <- liftIO getClockTime
        writeMS (t, d)
    , moduleExit = do
        (initial, d) <- readMS
        now          <- liftIO getClockTime
        writeMS (initial, max d (diffClockTimes now initial))
    }

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

doSystem :: String -> String -> Cmd System ()
doSystem cmd rest = withMsg $ \msg -> do
    target <- getTarget
    lift (doSystem' msg target cmd rest) >>= mapM_ say

doSystem' :: Msg.Message a => a -> Msg.Nick -> [Char] -> [Char] -> System [String]
doSystem' msg target cmd rest = get >>= \s -> case cmd of
  "listchans"   -> return [pprKeys (ircChannels s)]
  "listmodules" -> return [pprKeys (ircModules s) ]
  "listservers" -> return [pprKeys (ircServerMap s)]
  "listall"     -> lift listAll
  "list"| null rest         -> return ["What module?  (\"modules\" to list modules, \"all\" to list all commands)"]
        | rest == "all"     -> lift listAll
        | rest == "modules" -> return [pprKeys (ircModules s) ]
        | otherwise         -> lift $ listModule rest >>= return . (:[])

  ------------------------------------------------------------------------

  --TODO error handling
   -- system commands
  "join"  -> lift $ send (Msg.joinChannel (Msg.readNick msg rest)) >> return []
  "leave" -> lift $ send (Msg.partChannel (Msg.readNick msg rest)) >> return []
  "part"  -> lift $ send (Msg.partChannel (Msg.readNick msg rest)) >> return []

   -- writes to another location:
  "msg"   -> lift $ ircPrivmsg (Msg.readNick msg tgt) txt' >> return []
                  where (tgt, txt) = breakOnGlue " " rest
                        txt'       = dropWhile (== ' ') txt

  "quit" -> lift $ do ircQuit (Msg.server msg) $ if null rest then "requested" else rest
                      return []

  "reconnect" -> lift $ do ircReconnect (Msg.server msg) $ if null rest then "request" else rest
                           return []

  "echo" -> return [concat ["echo; msg:", show msg, " target:" , show target, " rest:", show rest]]

  "flush" -> lift $ do flushModuleState
                       return []

  "admin" -> do let pu = ircPrivilegedUsers s
                pu' <- case rest of '+':' ':_ -> return $ M.insert nck True pu
                                    '-':' ':_ -> return $ M.delete nck pu
                                    _         -> fail "@admin: invalid usage"
                put (s {ircPrivilegedUsers = pu'})
                return []
      where nck = Msg.readNick msg (drop 2 rest)

  "ignore" -> do let iu = ircIgnoredUsers s
                 iu' <- case rest of '+':' ':_ -> return $ M.insert nck True iu
                                     '-':' ':_ -> return $ M.delete nck iu
                                     _         -> fail "@ignore: invalid usage"
                 put (s {ircIgnoredUsers = iu'})
                 return []
      where nck = Msg.readNick msg (drop 2 rest)

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
listModule s = withModule s fromCommand printProvides
  where
    fromCommand = withCommand s
        (return $ "No module \""++s++"\" loaded") (const . printProvides)

    -- ghc now needs a type annotation here
    printProvides :: Module st -> ModuleT st LB String
    printProvides m = do
        cmds <- moduleCmds m
        let cmds' = filter (not . privileged) cmds
        name' <- getModuleName
        return . concat $ if null cmds'
                          then [name', " has no visible commands"]
                          else [name', " provides: ", showClean (concatMap cmdNames cmds')]

