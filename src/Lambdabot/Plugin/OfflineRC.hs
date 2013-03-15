-- | Offline mode / RC file / -e support module.  Handles spooling lists
-- of commands (from haskeline, files, or the command line) into the vchat
-- layer.
module Lambdabot.Plugin.OfflineRC ( theModule ) where

import Lambdabot.Config.Core
import Lambdabot.IRC
import Lambdabot.Monad
import Lambdabot.Plugin
import Lambdabot.Util.Error( finallyError )

import Control.Concurrent( forkIO )
import Control.Concurrent.MVar( readMVar )
import Control.Exception ( evaluate )
import Control.Monad( when )
import Control.Monad.Reader( asks )
import Control.Monad.State( gets )
import Control.Monad.Trans( lift, liftIO )
import Data.Char
import System.Console.Haskeline
import System.IO
import System.Timeout

-- We need to track the number of active sourcings so that we can
-- unregister the server (-> allow the bot to quit) when it is not
-- being used.
type OfflineRCState = Integer
type OfflineRC = ModuleT OfflineRCState LB

theModule :: Module OfflineRCState
theModule = newModule
    { moduleDefState = return 0
    , moduleInit = do
        act <- bindModule0 onInit
        _ <- lift $ liftLB forkIO $ do
                     mv <- asks ircInitDoneMVar
                     io $ readMVar mv
                     act
        return ()

    , moduleCmds = return
        [ (command "offline")
            { privileged = True
            , help = say "offline. Start a repl"
            , process = const . lift $ do
                act <- bindModule0 $ finallyError (runInputT defaultSettings replLoop) unlockRC
                lockRC
                _ <- lift $ liftLB forkIO act
                return ()
            }
        , (command "rc")
            { privileged = True
            , help = say "rc name. Read a file of commands (asynchonously). TODO: better name."
            , process = \fn -> lift $ do
                txt <- io $ readFile fn
                io $ evaluate $ foldr seq () txt
                act <- bindModule0 $ finallyError (mapM_ feed $ lines txt) unlockRC
                lockRC
                _ <- lift $ liftLB forkIO act
                return ()
            }
        ]
    }

onInit :: OfflineRC ()
onInit = do
    lockRC
    cmds <- getConfig onStartupCmds
    finallyError (mapM_ feed cmds) unlockRC

feed :: String -> OfflineRC ()
feed msg = do
    cmdPrefix <- fmap head (getConfig commandPrefixes)
    let msg' = case msg of
            '>':xs -> cmdPrefix ++ "run " ++ xs
            '!':xs -> xs
            _      -> cmdPrefix ++ dropWhile (== ' ') msg
    lift . (>> return ()) . liftLB (timeout (15 * 1000 * 1000)) . received $
              IrcMessage { ircMsgServer = "offlinerc"
                         , ircMsgLBName = "offline"
                         , ircMsgPrefix = "null!n=user@null"
                         , ircMsgCommand = "PRIVMSG"
                         , ircMsgParams = ["offline", ":" ++ msg' ] }

handleMsg :: IrcMessage -> LB ()
handleMsg msg = liftIO $ do
    let str = case (tail . ircMsgParams) msg of
            []    -> []
            (x:_) -> tail x
    hPutStrLn stdout str
    hFlush stdout

replLoop :: InputT OfflineRC ()
replLoop = do
    line <- getInputLine "lambdabot> "
    case line of
        Nothing -> return ()
        Just x -> do
            let s' = dropWhile isSpace x
            when (not $ null s') $ do
                lift $ feed s'
            continue <- lift (gets ircStayConnected)
            when continue replLoop

lockRC :: OfflineRC ()
lockRC = do
    add <- bindModule0 $ addServer "offlinerc" handleMsg
    withMS $ \ cur writ -> do
        when (cur == 0) $ add
        writ (cur + 1)

unlockRC :: OfflineRC ()
unlockRC = withMS $ \ cur writ -> do
    when (cur == 1) $ remServer "offlinerc"
    writ (cur - 1)
