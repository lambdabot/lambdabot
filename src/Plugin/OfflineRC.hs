-- | Offline mode / RC file / -e support module.  Handles spooling lists
-- of commands (from haskeline, files, or the command line) into the vchat
-- layer.
module Plugin.OfflineRC (theModule) where

import Plugin
import Lambdabot

import LMain( received )
import Control.Monad.Reader( asks )
import Control.Monad.State( get, gets, put )
import Control.Concurrent( forkIO )
import Control.Concurrent.MVar( readMVar )
import Lambdabot.Error( finallyError )
import Control.Exception ( evaluate )
import System.Console.Haskeline
import System.Console.Haskeline.History as Hist ( addHistory )

-- We need to track the number of active sourcings so that we can
-- unregister the server (-> allow the bot to quit) when it is not
-- being used.
type OfflineRCState = Integer
type OfflineRC = ModuleT OfflineRCState LB

theModule = newModule
    { moduleDefState = return 0
    , moduleInit = do
        act <- bindModule0 onInit
        lift $ liftLB forkIO $ do
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
                lift $ liftLB forkIO act
                return ()
            }
        , (command "rc")
            { privileged = True
            , help = say "rc name. Read a file of commands (asynchonously). FIXME: better name."
            , process = \fn -> lift $ do
                txt <- io $ readFile fn
                io $ evaluate $ foldr seq () txt
                act <- bindModule0 $ finallyError (mapM_ feed $ lines txt) unlockRC
                lockRC
                lift $ liftLB forkIO act
                return ()
            }
        ]
    }

onInit :: OfflineRC ()
onInit = do 
    st <- get
    put st { ircOnStartupCmds = [] }
    let cmds = ircOnStartupCmds st
    lockRC
    finallyError (mapM_ feed cmds) unlockRC

feed :: String -> OfflineRC ()
feed msg = let msg' = case msg of '>':xs -> cmdPrefix ++ "run " ++ xs
                                  '!':xs -> xs
                                  _      -> cmdPrefix ++ dropWhile (== ' ') msg
           in lift . (>> return ()) . liftLB (timeout (15 * 1000 * 1000)) . received $
              IrcMessage { ircMsgServer = "offlinerc"
                         , ircMsgLBName = "offline"
                         , ircMsgPrefix = "null!n=user@null"
                         , ircMsgCommand = "PRIVMSG"
                         , ircMsgParams = ["offline", ":" ++ msg' ] }
   where cmdPrefix = head (commandPrefixes config)

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
    s' <- case line of Nothing -> fail "<eof>"
                       Just x -> return $ dropWhile isSpace x
    when (not $ null s') $ do
        modifyHistory (addHistory s')
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
