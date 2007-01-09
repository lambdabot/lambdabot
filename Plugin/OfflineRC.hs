--
-- | Offline mode / RC file / -e support module.  Handles spooling lists
-- of commands (from readline, files, or the command line) into the vchat
-- layer.
--
module Plugin.OfflineRC (theModule) where

import Plugin
import System.IO( hPutStrLn, hFlush, stdout )

import LMain( received )
import IRCBase
import Control.Monad.Reader( asks )
import Control.Monad.State( get, put )
import Control.Concurrent( forkIO )
import Control.Concurrent.MVar( readMVar )
import Lib.Error( finallyError )

PLUGIN OfflineRC

-- We need to track the number of active sourcings so that we can
-- unregister the server (-> allow the bot to quit) when it is not
-- being used.
type OfflineRC = ModuleT Integer LB 

instance Module OfflineRCModule Integer where
    moduleInit      _ = do act <- bindModule0 onInit
                           lift $ liftLB forkIO $ do mv <- asks ircInitDoneMVar
                                                     io $ readMVar mv
                                                     act
                           return ()
    moduleDefState _  = return 0

onInit :: ModuleT Integer LB ()
onInit = do st <- get
            put (st { ircOnStartupCmds = [] })
            let cmds = ircOnStartupCmds st
            when (cmds /= []) (lockRC >> finallyError (mapM_ feed cmds) unlockRC)

feed :: String -> ModuleT Integer LB ()
feed msg = let msg' = case msg of '>':xs -> "@run " ++ xs
                                  '!':xs -> xs
                                  _      -> "@"     ++ dropWhile (== ' ') msg
           in lift $ (>> return ()) $ liftLB (timeout (15 * 1000 * 1000)) $ received $
              IrcMessage { msgServer = "offlinerc"
                         , msgLBName = "offline"
                         , msgPrefix = "null!n=user@null"
                         , msgCommand = "PRIVMSG"
                         , msgParams = ["offline", ":" ++ msg' ] }

handleMsg :: IrcMessage -> LB ()
handleMsg msg = liftIO $ do
                  let str = case (tail . msgParams) msg of
                              []    -> []
                              (x:_) -> tail x
                  hPutStrLn stdout str
                  hFlush stdout


lockRC :: OfflineRC ()
lockRC = do add <- bindModule0 $ addServer "offlinerc" handleMsg
            withMS $ \ cur writ -> do when (cur == 0) $ add
                                      writ (cur + 1)

unlockRC :: OfflineRC ()
unlockRC = withMS $ \ cur writ -> do when (cur == 1) $ remServer "offlinerc"
                                     writ (cur - 1)
