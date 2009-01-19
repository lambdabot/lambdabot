{-# LANGUAGE CPP, ExistentialQuantification, FlexibleContexts,
  FunctionalDependencies, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  PatternGuards, RankNTypes, TypeOperators #-}
-- | The guts of lambdabot.
--
-- The LB/Lambdabot monad
-- Generic server connection,disconnection
-- The module typeclass, type and operations on modules
module Lambdabot (
        MODULE(..), Module(..),
        ModuleT, ModuleLB, ModuleUnit, Mode(..),

        IRCRState(..), IRCRWState(..), IRCError(..),

        LB(..), lbIO,

        withModule, withAllModules, getDictKeys,

        getRef, getName, bindModule0, bindModule1, bindModule2,

        send, addServer, remServer, addServer',
        ircPrivmsg, ircPrivmsg', -- not generally used
        ircPrivmsgF,

        ircQuit, ircReconnect,
        ircGetChannels,
        ircSignalConnect, Callback, ircInstallOutputFilter, OutputFilter,
        ircInstallModule, ircUnloadModule,
        flushModuleState,

        ircLoad, ircUnload,

        checkPrivs, checkIgnore, mkCN, handleIrc, catchIrc, runIrc,
  ) where

import File (findFile)

import qualified Message as Msg
import qualified Shared  as S
import qualified IRCBase as IRC (IrcMessage, quit, privmsg)

import Lambdabot.Signals
import Lambdabot.Util
import Lambdabot.Serial

import Prelude hiding           (mod, catch)

import Network                  (withSocketsDo)

import System.Exit
import System.IO
import System.IO.Unsafe

#ifndef mingw32_HOST_OS
import System.Posix.Signals

-- n.b comment this out for prof
import System.Posix.Process     ( exitImmediately )
#endif

import Data.Char
import Data.IORef               (newIORef, IORef, readIORef, writeIORef)
import Data.List                (isSuffixOf, inits, tails)
import Data.Maybe               (isJust)
import Data.Map (Map)
import qualified Data.Map as M hiding (Map)
import qualified Data.ByteString.Char8 as P
import Data.ByteString (ByteString)

import Control.Concurrent (myThreadId, newEmptyMVar, newMVar, readMVar, putMVar,
                           takeMVar, threadDelay, MVar, ThreadId)
import Control.OldException
import Control.Monad.Error (MonadError (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans      ( liftIO )

import GHC.Err

#ifdef mingw32_HOST_OS
-- compatability shim
exitImmediately :: ExitCode -> IO a
exitImmediately = exitWith
#endif

------------------------------------------------------------------------
--
-- Lambdabot state
--

-- | Global read-only state.
data IRCRState
  = IRCRState {
        ircMainThread  :: ThreadId,
        ircInitDoneMVar:: MVar (),
        ircQuitMVar    :: MVar ()
        -- ^This is a mildly annoying hack.  In order to prevent the program
        -- from closing immediately, we have to keep the main thread alive, but
        -- the obvious infinite-MVar-wait technique doesn't work - the garbage
        -- collector helpfully notices that the MVar is dead, kills the main
        -- thread (which will never wake up, so why keep it around), thus
        -- terminating the program.  Behold the infinite wisdom that is the
        -- Glasgow FP group.
  }

type Callback = IRC.IrcMessage -> LB ()

type OutputFilter = Msg.Nick -> [String] -> LB [String]

-- | Global read\/write state.
data IRCRWState = IRCRWState {
        ircServerMap       :: Map String (String, IRC.IrcMessage -> LB ()),
        ircPrivilegedUsers :: Map Msg.Nick Bool,
        ircIgnoredUsers    :: Map Msg.Nick Bool,

        ircChannels        :: Map ChanName String,
            -- ^ maps channel names to topics

        ircModules         :: Map String ModuleRef,
        ircCallbacks       :: Map String [(String,Callback)],
        ircOutputFilters   :: [(String,OutputFilter)],
            -- ^ Output filters, invoked from right to left

        ircCommands        :: Map String ModuleRef,
        ircPrivCommands    :: [String],
        ircStayConnected   :: !Bool,
        ircDynLoad         :: S.DynLoad,
        ircOnStartupCmds   :: [String],
        ircPlugins         :: [String]
    }

-- The virtual chat system.
--
-- The virtual chat system sits between the chat drivers and the rest of
-- Lambdabot.  It provides a mapping between the String server "tags" and
-- functions which are able to handle sending messages.
--
-- When a message is recieved, the chat module is expected to call
-- `LMain.received'.  This is not ideal.

addServer :: String -> (IRC.IrcMessage -> LB ()) -> ModuleT s LB ()
addServer tag sendf = do
    s <- get
    let svrs = ircServerMap s
    name <- getName
    case M.lookup tag svrs of
        Nothing -> put (s { ircServerMap = M.insert tag (name,sendf) svrs})
        Just _ -> fail $ "attempted to create two servers named " ++ tag

-- This is a crutch until all the servers are pluginized.
addServer' :: String -> (IRC.IrcMessage -> LB ()) -> LB ()
addServer' tag sendf = do
    s <- get
    let svrs = ircServerMap s
    case M.lookup tag svrs of
        Nothing -> put (s { ircServerMap = M.insert tag ("<core>",sendf) svrs})
        Just _ -> fail $ "attempted to create two servers named " ++ tag

remServer :: String -> LB ()
remServer tag = do
    s <- get
    let svrs = ircServerMap s
    case M.lookup tag svrs of
        Just _ -> do let svrs' = M.delete tag svrs
                     main <- asks ircMainThread
                     when (M.null svrs') $ io $ throwTo main (ErrorCall "all servers detached")
                     put (s { ircServerMap = svrs' })
        Nothing -> fail $ "attempted to delete nonexistent servers named " ++ tag

send :: IRC.IrcMessage -> LB ()
send msg = do
    s <- gets ircServerMap
    case M.lookup (Msg.server msg) s of
        Just (_, sendf) -> sendf msg
        Nothing -> io $ hPutStrLn stderr $ "sending message to bogus server: " ++ show msg

newtype ChanName = ChanName { getCN :: Msg.Nick } -- should be abstract, always lowercase
  deriving (Eq, Ord)

instance Show ChanName where show (ChanName x) = show x

-- | only use the "smart constructor":
mkCN :: Msg.Nick -> ChanName
mkCN = ChanName . liftM2 Msg.Nick Msg.nTag (map toLower . Msg.nName)

-- ---------------------------------------------------------------------
--
-- The LB (LambdaBot) monad
--

-- | The IRC Monad. The reader transformer holds information about the
--   connection to the IRC server.
--
-- instances Monad, Functor, MonadIO, MonadState, MonadError


newtype LB a = LB { runLB :: ReaderT (IRCRState,IORef IRCRWState) IO a }
    deriving (Monad,Functor,MonadIO)

-- Actually, this isn't a reader anymore
instance MonadReader IRCRState LB where
    ask   = LB $ asks fst
    local = error "You are not supposed to call local"

instance MonadState IRCRWState LB where
    get = LB $ do
        ref <- asks snd
        lift $ readIORef ref
    put x = LB $ do
        ref <- asks snd
        lift $ writeIORef ref x

-- And now a MonadError instance to map IRCErrors to MonadError in LB,
-- so throwError and catchError "just work"
instance MonadError IRCError LB where
  throwError (IRCRaised e)    = io $ throwIO e
  throwError (SignalCaught e) = io $ evaluate (throwDyn $ SignalException e)
  m `catchError` h = lbIO $ \conv -> (conv m
              `catchDyn` \(SignalException e) -> conv $ h $ SignalCaught e)
              `catch` \e -> conv $ h $ IRCRaised e

-- A type for handling both Haskell exceptions and external signals
data IRCError = IRCRaised Exception | SignalCaught Signal

instance Show IRCError where
    show (IRCRaised    e) = show e
    show (SignalCaught s) = show s

-- lbIO return :: LB (LB a -> IO a)
-- CPS to work around predicativiy of haskell's type system.
lbIO :: ((forall a. LB a -> IO a) -> IO b) -> LB b
lbIO k = LB . ReaderT $ \r -> k (\(LB m) -> m `runReaderT` r)

-- | run a computation in the LB monad
evalLB :: LB a -> IRCRState -> IRCRWState -> IO a
evalLB (LB lb) rs rws = do
    ref  <- newIORef rws
    lb `runReaderT` (rs,ref)

-- May wish to add more things to the things caught, or restructure things
-- a bit. Can't just catch everything - in particular EOFs from the socket
-- loops get thrown to this thread and we musn't just ignore them.
handleIrc :: MonadError IRCError m => (IRCError -> m ()) -> m () -> m ()
handleIrc handler m = catchError m handler

-- Like handleIrc, but with arguments reversed
catchIrc :: MonadError IRCError m => m () -> (IRCError -> m ()) -> m ()
catchIrc = flip handleIrc

------------------------------------------------------------------------
--
-- Lambdabot modes, networked , or command line
--
data Mode = Online | Offline deriving Eq

-- | The Lambdabot entry point.
-- Initialise plugins, connect, and run the bot in the LB monad
--
-- Also, handle any fatal exceptions (such as non-recoverable signals),
-- (i.e. print a message and exit). Non-fatal exceptions should be dealt
-- with in the mainLoop or further down.
runIrc :: [String] -> LB a -> S.DynLoad -> [String] -> IO ()
runIrc evcmds initialise ld plugins = withSocketsDo $ do
    rost <- initRoState
    r <- try $ evalLB (do withDebug "Initialising plugins" initialise
                          withIrcSignalCatch mainLoop)
                       rost (initState ld plugins evcmds)

    -- clean up and go home
    case r of
        Left er -> do putStrLn "exception:"
                      print er
                      exitWith (ExitFailure 1) -- won't happen.  exitImmediately cleans it all up
        Right _ -> exitWith ExitSuccess

--
-- | Default ro state
--
initRoState :: IO IRCRState
initRoState = do
    threadmain <- io myThreadId
    quitMVar <- io newEmptyMVar
    initDoneMVar <- io newEmptyMVar

    return $ IRCRState {
                 ircQuitMVar    = quitMVar,
                 ircInitDoneMVar= initDoneMVar,
                 ircMainThread  = threadmain
             }

--
-- | Default rw state
--
initState :: S.DynLoad -> [String] -> [String] -> IRCRWState
initState ld plugins evcmds = IRCRWState {
        ircPrivilegedUsers = M.singleton (Msg.Nick "offlinerc" "null") True,
        ircIgnoredUsers    = M.empty,
        ircChannels        = M.empty,
        ircModules         = M.empty,
        ircServerMap       = M.empty,
        ircCallbacks       = M.empty,
        ircOutputFilters   = [
            ([],cleanOutput),
            ([],lineify),
            ([],cleanOutput),
        --  ([],reduceIndent),
            ([],checkRecip) ],
        ircCommands        = M.empty,
        ircPrivCommands    = [],
        ircStayConnected   = True,
        ircDynLoad         = ld,
        ircPlugins         = plugins,
        ircOnStartupCmds   = evcmds
    }

-- Actually, this isn't a loop anymore.  FIXME: better name.
mainLoop :: LB ()
mainLoop = do

    catchIrc
       (do asks ircInitDoneMVar >>= io . flip putMVar ()
           asks ircQuitMVar >>= io . takeMVar
           fail "don't write to the quitMVar!")
       (\e -> do -- catch anything, print informative message, and clean up
            io $ hPutStrLn stderr $
                       (case e of
                            IRCRaised ex   -> "Exception: " ++ show ex
                            SignalCaught s -> "Signal: " ++ ircSignalMessage s)
        --  withDebug "Running exit handlers"    runExitHandlers
        --  withDebug "Writing persistent state" flushModuleState
            runExitHandlers >> flushModuleState

      -- this kills profiling output:
            io $ exitImmediately (ExitFailure 1))

      --    throwError e)

-- | run 'exit' handler on modules
runExitHandlers:: LB ()
runExitHandlers = withAllModules moduleExit >> return ()

-- | flush state of modules
flushModuleState :: LB ()
flushModuleState = do
    withAllModules (\m -> getName >>= writeGlobalState m)
    return ()

------------------------------------------------------------------------

-- | The Module type class.
-- Minimal complete definition: @moduleHelp@, @moduleCmds@, and
-- either @process@ or @process_@
class Module m s | m -> s where
    -- | If the module wants its state to be saved, this function should
    --   return a Serial.
    --
    --   The default implementation returns Nothing.
    moduleSerialize :: m -> Maybe (Serial s)

    -- | If the module maintains state, this method specifies the default state
    --   (for example in case the state can't be read from a state).
    --
    --   The default implementation returns an error and assumes the state is
    --   never accessed.
    moduleDefState  :: m -> LB s

    -- | Is the module sticky? Sticky modules (as well as static ones) can't be
    --   unloaded. By default, modules are not sticky.
    moduleSticky    :: m -> Bool

    -- | The commands the module listenes to.
    moduleCmds      :: m -> [String]

    -- | This method should return a help string for every command it defines.
    moduleHelp      :: m -> String -> String

    -- | The privileged commands the module listenes to.
    modulePrivs     :: m -> [String]

    -- | Initialize the module. The default implementation does nothing.
    moduleInit      :: m -> ModuleT s LB ()

    -- | Finalize the module. The default implementation does nothing.
    moduleExit      :: m -> ModuleT s LB ()

    -- | Process a command a user sent, the resulting string is draw in
    -- some fashion. If the `process' function doesn't exist, we catch
    -- an exception when we try to call it, and instead call `process_'
    -- which is guaranteed to at least have a default instance.
    -- This magic (well, for Haskell) occurs in Base.hs
    --
    process :: Msg.Message a
        => m                                -- ^ phantom     (required)
        -> a                                -- ^ the message (uneeded by most?)
        -> Msg.Nick                         -- ^ target
        -> String                           -- ^ command
        -> String                           -- ^ the arguments to the command
        -> ModuleLB s                       -- ^ maybe output

    -- | Process contextual input. A plugin that implements 'contextual'
    -- is able to respond to text not part of a normal command.
    contextual :: Msg.Message a
        => m                                -- ^ phantom     (required)
        -> a                                -- ^ the message
        -> Msg.Nick                         -- ^ target
        -> String                           -- ^ the text
        -> ModuleLB s                       -- ^ maybe output

    -- | Like process, but uncommonly used args are ignored
    -- Lambdabot will attempt to run process first, and then fall back
    -- to process_, which in turn has a default instance.
    --
    process_ :: m                           -- ^ phantom
             -> String -> String            -- ^ command, args
             -> ModuleLB s                  -- ^ maybe output

    -- A bytestring version
    --
    fprocess_ :: m                        -- ^ phantom
              -> ByteString -> ByteString -- ^ command, args
              -> ModuleF s                -- ^ maybe output

------------------------------------------------------------------------

    contextual _ _ _ _ = return []
    process_ _ _ _     = return []

    moduleHelp m _     = concat (map ('@':) (moduleCmds m))
    modulePrivs _      = []
    moduleCmds      _  = []
    moduleExit _       = return ()
    moduleInit _       = return ()
    moduleSticky _     = False
    moduleSerialize _  = Nothing
    moduleDefState  _  = return $ error "state not initialized"

-- | An existential type holding a module, used to represent modules on
-- the value level, for manipluation at runtime by the dynamic linker.
data MODULE = forall m s. (Module m s) => MODULE m

data ModuleRef = forall m s. (Module m s) => ModuleRef m (MVar s) String

--
-- | This transformer encodes the additional information a module might
--   need to access its name or its state.
--
newtype ModuleT s m a = ModuleT { moduleT :: ReaderT (MVar s, String) m a }
    deriving (Functor, Monad, MonadTrans, MonadIO, MonadError e, MonadState t)

getRef :: Monad m => ModuleT s m (MVar s)
getRef  = ModuleT $ ask >>= return . fst

getName :: Monad m => ModuleT s m String
getName = ModuleT $ ask >>= return . snd

-- | bind an action to the current module so it can be run from the plain
--   `LB' monad.
bindModule0 :: ModuleT s LB a -> ModuleT s LB (LB a)
bindModule0 act = bindModule1 (const act) >>= return . ($ ())

-- | variant of `bindModule0' for monad actions with one argument
bindModule1 :: (a -> ModuleT s LB b) -> ModuleT s LB (a -> LB b)
bindModule1 act = ModuleT $
    ask >>= \st -> return (\val -> runReaderT (moduleT $ act val) st)

-- | variant of `bindModule0' for monad actions with two arguments
bindModule2 :: (a -> b -> ModuleT s LB c) -> ModuleT s LB (a -> b -> LB c)
bindModule2 act = bindModule1 (uncurry act) >>= return . curry

-- | A nicer synonym for some ModuleT stuffs
type ModuleLB s = ModuleT s LB [String]

-- | And for packed output
type ModuleF  s = ModuleT s LB [ByteString]

type ModuleUnit s = ModuleT s LB ()

-- ---------------------------------------------------------------------
--
-- Handling global state
--

-- | Peristence: write the global state out
writeGlobalState :: Module m s => m -> String -> ModuleT s LB ()
writeGlobalState mod name = case moduleSerialize mod of
  Nothing  -> return ()
  Just ser -> do
    state <- getRef >>= (io . readMVar) -- readMS
    case serialize ser state of
        Nothing  -> return ()   -- do not write any state
        Just out -> io $ P.writeFile (toFilename name) out

-- | Read it in
readGlobalState :: Module m s => m -> String -> IO (Maybe s)
readGlobalState mod name
    | Just ser <- moduleSerialize mod  = do
        state <- Just `fmap` P.readFile (toFilename name) `catch` \_ -> return Nothing
        catch (evaluate $ maybe Nothing (Just $!) (deserialize ser =<< state)) -- Monad Maybe)
              (\e -> do hPutStrLn stderr $ "Error parsing state file for: "
                                        ++ name ++ ": " ++ show e
                        hPutStrLn stderr $ "Try removing: "++ show (toFilename name)
                        return Nothing) -- proceed regardless
    | otherwise = return Nothing

-- | helper
toFilename :: String -> String
toFilename = unsafePerformIO . findFile

------------------------------------------------------------------------
--
-- | Register a module in the irc state
--
ircInstallModule :: MODULE -> String -> LB ()
ircInstallModule (MODULE mod) modname = do
    savedState <- io $ readGlobalState mod modname
    state      <- maybe (moduleDefState mod) return savedState
    ref        <- io $ newMVar state

    let modref = ModuleRef mod ref modname

    flip runReaderT (ref, modname) . moduleT $ do
        moduleInit mod
        let cmds  = moduleCmds mod
            privs = modulePrivs mod

        s <- get
        let modmap = ircModules s
            cmdmap = ircCommands s
        put $ s {
          ircModules = M.insert modname modref modmap,
          ircCommands = addList [ (cmd,modref) | cmd <- cmds++privs ] cmdmap,
          ircPrivCommands = ircPrivCommands s ++ privs
        }
        io $ hPutStr stderr "." >> hFlush stderr

--
-- | Unregister a module's entry in the irc state
--
ircUnloadModule :: String -> LB ()
ircUnloadModule modname = withModule ircModules modname (error "module not loaded") (\m -> do
    when (moduleSticky m) $ error "module is sticky"
    moduleExit m
    writeGlobalState m modname
    s <- get
    let modmap = ircModules s
        cmdmap = ircCommands s
        cbs    = ircCallbacks s
        svrs   = ircServerMap s
        ofs    = ircOutputFilters s
    put $ s { ircCommands      = M.filter (\(ModuleRef _ _ name) -> name /= modname) cmdmap }
            { ircModules       = M.delete modname modmap }
            { ircCallbacks     = filter ((/=modname) . fst) `fmap` cbs }
            { ircServerMap     = M.filter ((/=modname) . fst) svrs }
            { ircOutputFilters = filter ((/=modname) . fst) ofs }
  )

--
-- | Binding to dynamic loader functions (stored as a bundle in state)
-- passed from Boot. DynamicModule goes through here to get at them.
--
ircLoad :: FilePath -> S.Symbol -> LB (S.Module, a)
ircLoad mod sym = do
    s <- get
    let fn  = S.dynload (ircDynLoad s)
    io $ (fn mod sym)

--
-- | Dynamically unload a module
--
ircUnload :: FilePath -> LB ()
ircUnload mod = do
    s <- get
    io $ (S.unload (ircDynLoad s)) (S.Module mod)

------------------------------------------------------------------------

ircSignalConnect :: String -> Callback -> ModuleT s LB ()
ircSignalConnect str f = do
    s <- get
    let cbs = ircCallbacks s
    name <- getName
    case M.lookup str cbs of -- TODO
        Nothing -> put (s { ircCallbacks = M.insert str [(name,f)]    cbs})
        Just fs -> put (s { ircCallbacks = M.insert str ((name,f):fs) cbs})

ircInstallOutputFilter :: OutputFilter -> ModuleT s LB ()
ircInstallOutputFilter f = do
    name <- getName
    modify $ \s ->
        s { ircOutputFilters = (name, f): ircOutputFilters s }

-- | Checks if the given user has admin permissions and excecute the action
--   only in this case.
checkPrivs :: IRC.IrcMessage -> LB Bool
checkPrivs msg = gets (isJust . M.lookup (Msg.nick msg) . ircPrivilegedUsers)

-- | Checks if the given user is being ignored.
--   Privileged users can't be ignored.
checkIgnore :: IRC.IrcMessage -> LB Bool
checkIgnore msg = liftM2 (&&) (liftM not (checkPrivs msg))
                  (gets (isJust . M.lookup (Msg.nick msg) . ircIgnoredUsers))

------------------------------------------------------------------------
-- Some generic server operations

ircGetChannels :: LB [Msg.Nick]
ircGetChannels = (map getCN . M.keys) `fmap` gets ircChannels

-- Send a quit message, settle and wait for the server to drop our
-- handle. At which point the main thread gets a closed handle eof
-- exceptoin, we clean up and go home
ircQuit :: String -> String -> LB ()
ircQuit svr msg = do
    modify $ \state -> state { ircStayConnected = False }
    send  $ IRC.quit svr msg
    liftIO $ threadDelay 1000
    io $ hPutStrLn stderr "Quit"

ircReconnect :: String -> String -> LB ()
ircReconnect svr msg = do
    send $ IRC.quit svr msg
    liftIO $ threadDelay 1000

-- | Send a message to a channel\/user. If the message is too long, the rest
--   of it is saved in the (global) more-state.
ircPrivmsg :: Msg.Nick      -- ^ The channel\/user.
           -> String        -- ^ The message.
           -> LB ()

ircPrivmsg who msg = do
    filters   <- gets ircOutputFilters
    sendlines <- foldr (\f -> (=<<) (f who)) ((return . lines) msg) $ map snd filters
    mapM_ (\s -> ircPrivmsg' who (take textwidth s)) (take 10 sendlines)

-- A raw send version
ircPrivmsg' :: Msg.Nick -> String -> LB ()
ircPrivmsg' who ""  = ircPrivmsg' who " "
ircPrivmsg' who msg = send $ IRC.privmsg who msg

----------------------------------------------------------------------------------

ircPrivmsgF :: Msg.Nick -> ByteString -> LB ()
ircPrivmsgF who s= ircPrivmsg who (P.unpack s) -- TODO

{-
rawPrivmsgF :: String -> Maybe ByteString -> LB ()
rawPrivmsgF _   Nothing  = send Nothing
rawPrivmsgF who (Just s) | P.null s  = ircPrivmsg' who (Just " ")
                         | otherwise = send . Just $ IRC.privmsgF who msg
-}

------------------------------------------------------------------------
-- Module handling

-- | Interpret an expression in the context of a module.
-- Arguments are which map to use (@ircModules@ and @ircCommands@ are
-- the only sensible arguments here), the name of the module\/command,
-- action for the case that the lookup fails, action if the lookup
-- succeeds.
--
withModule :: (Ord k)
           => (IRCRWState -> Map k ModuleRef)
           -> k
           -> LB a
           -> (forall mod s. Module mod s => mod -> ModuleT s LB a)
           -> LB a

withModule dict modname def f = do
    maybemod <- gets (M.lookup modname . dict)
    case maybemod of
      -- TODO stick this ref stuff in a monad instead. more portable in
      -- the long run.
      Just (ModuleRef m ref name) -> do
          runReaderT (moduleT $ f m) (ref, name)
      _                           -> def

-- | Interpret a function in the context of all modules
withAllModules :: (forall mod s. Module mod s => mod -> ModuleT s LB a) -> LB [a]
withAllModules f = do
    mods <- gets $ M.elems . ircModules :: LB [ModuleRef]
    (`mapM` mods) $ \(ModuleRef m ref name) -> do
        runReaderT (moduleT $ f m) (ref, name)

getDictKeys :: (MonadState s m) => (s -> Map k a) -> m [k]
getDictKeys dict = gets (M.keys . dict)

------------------------------------------------------------------------

-- | Print a debug message, and perform an action
withDebug :: String -> LB a -> LB ()
withDebug s a = do
    io $ hPutStr stderr (s ++ " ...")  >> hFlush stderr
    a
    io $ hPutStrLn stderr " done." >> hFlush stderr

----------------------------------------------------------------------
-- Output filters

textwidth :: Int
textwidth = 200 -- IRC maximum msg length, minus a bit for safety.

-- | For now, this just checks for duplicate empty lines.
cleanOutput :: OutputFilter
cleanOutput _ msg = return $ remDups True msg'
    where
        remDups True  ([]:xs) =    remDups True xs
        remDups False ([]:xs) = []:remDups True xs
        remDups _     (x: xs) = x: remDups False xs
        remDups _     []      = []
        msg' = map dropSpaceEnd msg

-- | wrap long lines.
lineify :: OutputFilter
lineify = const (return . mlines . unlines)

-- | break into lines
mlines :: String -> [String]
mlines = (mbreak =<<) . lines
    where
        mbreak :: String -> [String]
        mbreak xs
            | null bs   = [as]
            | otherwise = (as++cs) : filter (not . null) (mbreak ds)
            where
                (as,bs) = splitAt (w-n) xs
                breaks  = filter (not . isAlphaNum . last . fst) $ drop 1 $
                                  take n $ zip (inits bs) (tails bs)
                (cs,ds) = last $ (take n bs, drop n bs): breaks
                w = textwidth
                n = 10

-- | Don't send any output to alleged bots.
checkRecip :: OutputFilter
checkRecip who msg
--  FIXME: this doesn't work with plugin protocols :(
--  | who == Config.name Config.config                   = return []
    | "bot" `isSuffixOf` lowerCaseString (Msg.nName who) = return []
    | otherwise                                          = return msg

-- | Divide the lines' indent by two
{-
reduceIndent :: OutputFilter
reduceIndent _ msg = return $ map redLine msg
    where
        redLine (' ':' ':xs)        = ' ':redLine xs
        redLine xs                  = xs
-}
