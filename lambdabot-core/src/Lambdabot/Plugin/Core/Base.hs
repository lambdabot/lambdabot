{-# LANGUAGE PatternGuards #-}
-- | Lambdabot base module. Controls message send and receive
module Lambdabot.Plugin.Core.Base (basePlugin) where

import Lambdabot.Bot
import Lambdabot.Command
import Lambdabot.Config.Core
import Lambdabot.IRC
import Lambdabot.Logging
import Lambdabot.Message
import Lambdabot.Monad
import Lambdabot.Nick
import Lambdabot.Plugin
import Lambdabot.Util

import Control.Applicative
import Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Text.EditDistance
import Text.Regex.TDFA

type BaseState = GlobalPrivate () ()
type Base = ModuleT BaseState LB

basePlugin :: Module (GlobalPrivate () ())
basePlugin = newModule
    { moduleDefState = return $ mkGlobalPrivate 20 ()
    , moduleInit = do
             ircSignalConnect "PING"    doPING
             bindModule1 doNOTICE >>= ircSignalConnect "NOTICE"
             ircSignalConnect "PART"    doPART
             bindModule1 doKICK   >>= ircSignalConnect "KICK"
             ircSignalConnect "JOIN"    doJOIN
             ircSignalConnect "NICK"    doNICK
             ircSignalConnect "MODE"    doMODE
             ircSignalConnect "TOPIC"   doTOPIC
             ircSignalConnect "QUIT"    doQUIT
             bindModule1 doPRIVMSG >>= ircSignalConnect "PRIVMSG"
             ircSignalConnect "001"     doRPL_WELCOME

          {- ircSignalConnect "002"     doRPL_YOURHOST
             ircSignalConnect "003"     doRPL_CREATED
             ircSignalConnect "004"     doRPL_MYINFO -}

             ircSignalConnect "005"     doRPL_BOUNCE

          {- ircSignalConnect "250"     doRPL_STATSCONN
             ircSignalConnect "251"     doRPL_LUSERCLIENT
             ircSignalConnect "252"     doRPL_LUSEROP
             ircSignalConnect "253"     doRPL_LUSERUNKNOWN
             ircSignalConnect "254"     doRPL_LUSERCHANNELS
             ircSignalConnect "255"     doRPL_LUSERME
             ircSignalConnect "265"     doRPL_LOCALUSERS
             ircSignalConnect "266"     doRPL_GLOBALUSERS -}

             ircSignalConnect "332"     doRPL_TOPIC

          {- ircSignalConnect "353"     doRPL_NAMRELY
             ircSignalConnect "366"     doRPL_ENDOFNAMES
             ircSignalConnect "372"     doRPL_MOTD
             ircSignalConnect "375"     doRPL_MOTDSTART
             ircSignalConnect "376"     doRPL_ENDOFMOTD -}
    }

doIGNORE :: Callback
doIGNORE = debugM . show

doPING :: Callback
doPING = noticeM . showPingMsg
    where showPingMsg msg = "PING! <" ++ ircMsgServer msg ++ (':' : ircMsgPrefix msg) ++
            "> [" ++ ircMsgCommand msg ++ "] " ++ show (ircMsgParams msg)

-- If this is a "TIME" then we need to pass it over to the localtime plugin
-- otherwise, dump it to stdout
doNOTICE :: IrcMessage -> Base ()
doNOTICE msg
    | isCTCPTimeReply   = doPRIVMSG (timeReply msg)
        -- TODO: need to say which module to run the privmsg in
    | otherwise         = noticeM (show body)
    where
        body = ircMsgParams msg
        isCTCPTimeReply = ":\SOHTIME" `isPrefixOf` (last body)

doJOIN :: Callback
doJOIN msg 
    | lambdabotName msg /= nick msg = doIGNORE msg
    | otherwise                     = do
        let msgArg  = concat (take 1 (ircMsgParams msg))
            chan    = case dropWhile (/= ':') msgArg of
                []      -> msgArg
                aloc    -> aloc
            loc = Nick (server msg) (dropWhile (== ':') chan)
        
        s <- get
        put (s { ircChannels = M.insert  (mkCN loc) "[currently unknown]" (ircChannels s)}) -- the empty topic causes problems
        send $ getTopic loc -- initialize topic
   where 

doPART :: Callback
doPART msg
  = when (lambdabotName msg == nick msg) $ do
        let body = ircMsgParams msg
            loc = Nick (server msg) (head body)
        s <- get
        put (s { ircChannels = M.delete (mkCN loc) (ircChannels s) })

doKICK :: IrcMessage -> Base ()
doKICK msg
   = do let body = ircMsgParams msg
            loc = Nick (server msg) (body !! 0)
            who = Nick (server msg) (body !! 1)
        when (lambdabotName msg == who) $ do
            noticeM $ fmtNick "" (nick msg) ++ " KICK " ++ fmtNick (server msg) loc ++ " " ++ show (drop 2 body)
            lift $ modify $ \s ->
                s { ircChannels = M.delete (mkCN loc) (ircChannels s) }

doNICK :: Callback
doNICK msg
  = doIGNORE msg

doMODE :: Callback
doMODE msg
  = doIGNORE msg


doTOPIC :: Callback
doTOPIC msg
    = do let loc = Nick (server msg) (head (ircMsgParams msg))
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ head $ tail $ ircMsgParams msg) (ircChannels s)})

doRPL_WELCOME :: Callback
doRPL_WELCOME = doIGNORE

doQUIT :: Callback
doQUIT msg = doIGNORE msg

doRPL_BOUNCE :: Callback
doRPL_BOUNCE _msg = debugM "BOUNCE!"

doRPL_TOPIC :: Callback
doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of body
    = do let body = ircMsgParams msg
             loc = Nick (server msg) (body !! 1)
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ last body) (ircChannels s) })

doPRIVMSG :: IrcMessage -> Base ()
doPRIVMSG msg = do
    ignored     <- lift $ checkIgnore msg
    commands    <- getConfig commandPrefixes
    
    if ignored
        then lift $ doIGNORE msg
        else mapM_ (doPRIVMSG' commands (lambdabotName msg) msg) targets
    where
        alltargets = head (ircMsgParams msg)
        targets = map (parseNick (ircMsgServer msg)) $ splitOn "," alltargets

--
-- | What does the bot respond to?
--
doPRIVMSG' :: [String] -> Nick -> IrcMessage -> Nick -> Base ()
doPRIVMSG' commands myname msg target
    | myname == target
    = let (cmd, params) = splitFirstWord text
      in doPersonalMsg commands msg target text cmd params
    
    | flip any ":," $ \c -> (fmtNick (ircMsgServer msg) myname ++ [c]) `isPrefixOf` text
    = let Just wholeCmd = maybeCommand (fmtNick (ircMsgServer msg) myname) text
          (cmd, params) = splitFirstWord wholeCmd
      in doPublicMsg commands msg target cmd params
    
    | (commands `arePrefixesOf` text)
    && length text > 1
    && (text !! 1 /= ' ') -- elem of prefixes
    && (not (commands `arePrefixesOf` [text !! 1]) ||
      (length text > 2 && text !! 2 == ' ')) -- ignore @@ prefix, but not the @@ command itself
    = let (cmd, params) = splitFirstWord (dropWhile (==' ') text)
      in doPublicMsg commands msg target cmd params
    
    | otherwise =  doContextualMsg msg target target text
    
    where
        text = tail (head (tail (ircMsgParams msg)))

doPersonalMsg :: [String] -> IrcMessage -> Nick -> String -> String -> String -> Base ()
doPersonalMsg commands msg target text s r
    | commands `arePrefixesOf` s  = doMsg msg (tail s) r who
    | otherwise                   = doContextualMsg msg target who text
    where
      who = nick msg

doPublicMsg :: [String] -> IrcMessage -> Nick -> String -> String -> Base ()
doPublicMsg commands msg target s r
    | commands `arePrefixesOf` s  = doMsg msg (tail s) r target
    | otherwise                   = (lift $ doIGNORE msg)

--
-- normal commands.
--
-- check privledges, do any spell correction, dispatch, handling
-- possible timeouts.
--
-- todo, refactor
--
doMsg :: IrcMessage -> String -> String -> Nick -> Base ()
doMsg msg cmd rest towhere = do
    let ircmsg = ircPrivmsg towhere
    allcmds <- lift (gets (M.keys . ircCommands))
    let ms      = filter (isPrefixOf cmd) allcmds
    e <- getConfig editDistanceLimit
    case ms of
        [s] -> docmd msg towhere rest s                  -- a unique prefix
        _ | cmd `elem` ms -> docmd msg towhere rest cmd  -- correct command (usual case)
        _ | otherwise     -> case closests cmd allcmds of
          (n,[s]) | n < e ,  ms == [] -> docmd msg towhere rest s -- unique edit match
          (n,ss)  | n < e || ms /= []            -- some possibilities
              -> lift . ircmsg $ "Maybe you meant: "++showClean(nub(ms++ss))
          _   -> docmd msg towhere rest cmd         -- no prefix, edit distance too far

docmd :: IrcMessage -> Nick -> [Char] -> String -> Base ()
docmd msg towhere rest cmd' = withPS towhere $ \_ _ -> do
    withCommand cmd'   -- Important.
        (ircPrivmsg towhere "Unknown command, try @list")
        (\_ theCmd -> do
            name'   <- getModuleName

            hasPrivs <- lb (checkPrivs msg)
            
            -- TODO: handle disabled commands earlier
            -- users should probably see no difference between a
            -- command that is disabled and one that doesn't exist.
            disabled <- elem cmd' <$> getConfig disabledCommands
            let ok = not disabled && (not (privileged theCmd) || hasPrivs)

            response <- if not ok
                then return ["Not enough privileges"]
                else runCommand theCmd msg towhere cmd' rest
                    `E.catch` \exc@SomeException{} ->
                        return ["Plugin `" ++ name' ++ "' failed with: " ++ show exc]
            
            -- send off our response strings
            -- TODO: expandTab here should probably be an OutputFilter
            lift $ mapM_ (ircPrivmsg towhere . expandTab 8) response
        )

--
-- contextual messages are all input that isn't an explicit command.
-- they're passed to all modules (todo, sounds inefficient) for
-- scanning, and any that implement 'contextual' will reply.
--
-- we try to run the contextual functions from all modules, on every
-- non-command. better hope this is efficient.
--
-- Note how we catch any plugin errors here, rather than letting
-- them bubble back up to the mainloop
--
doContextualMsg :: IrcMessage -> Nick -> Nick -> [Char] -> Base ()
doContextualMsg msg target towhere r = lift $ withAllModules $ \m -> do
    name' <- getModuleName
    E.catch
        (lift . mapM_ (ircPrivmsg towhere) =<< execCmd (contextual m r) msg target "contextual") 
        (\e@SomeException{} -> debugM . (name' ++) . (" module failed in contextual handler: " ++) $ show e)

------------------------------------------------------------------------

closests :: String -> [String] -> (Int,[String])
closests pat ss = M.findMin m
    where
        m = M.fromListWith (++) ls
        ls = [ (levenshteinDistance defaultEditCosts pat s, [s]) | s <- ss ]

maybeCommand :: String -> String -> Maybe String
maybeCommand nm text = mrAfter <$> matchM re text
    where
        re :: Regex
        re = makeRegex (nm ++ "[.:,]*[[:space:]]*")

--
-- And stuff we don't care about
--

{-
doRPL_YOURHOST :: Callback
doRPL_YOURHOST _msg = return ()

doRPL_CREATED :: Callback
doRPL_CREATED _msg = return ()

doRPL_MYINFO :: Callback
doRPL_MYINFO _msg = return ()

doRPL_STATSCONN :: Callback
doRPL_STATSCONN _msg = return ()

doRPL_LUSERCLIENT :: Callback
doRPL_LUSERCLIENT _msg = return ()

doRPL_LUSEROP :: Callback
doRPL_LUSEROP _msg = return ()

doRPL_LUSERUNKNOWN :: Callback
doRPL_LUSERUNKNOWN _msg = return ()

doRPL_LUSERCHANNELS :: Callback
doRPL_LUSERCHANNELS _msg = return ()

doRPL_LUSERME :: Callback
doRPL_LUSERME _msg = return ()

doRPL_LOCALUSERS :: Callback
doRPL_LOCALUSERS _msg = return ()

doRPL_GLOBALUSERS :: Callback
doRPL_GLOBALUSERS _msg = return ()

doUNKNOWN :: Callback
doUNKNOWN msg
    = debugM $ "UNKNOWN> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (body msg)

doRPL_NAMREPLY :: Callback
doRPL_NAMREPLY _msg = return ()

doRPL_ENDOFNAMES :: Callback
doRPL_ENDOFNAMES _msg = return ()

doRPL_MOTD :: Callback
doRPL_MOTD _msg = return ()

doRPL_MOTDSTART :: Callback
doRPL_MOTDSTART _msg = return ()

doRPL_ENDOFMOTD :: Callback
doRPL_ENDOFMOTD _msg = return ()
-}
