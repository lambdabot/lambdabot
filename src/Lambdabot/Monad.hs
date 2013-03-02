{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Lambdabot.Monad
    ( IRCRState(..)
    , IRCRWState(..)
    , ModuleRef(..)
    , CommandRef(..)
    , IRCError(..)
    
    , Callback
    , OutputFilter
    
    , ChanName
    , mkCN
    , getCN
    
    , LB(..)
    , MonadLB(..)
    , lbIO
    , evalLB
    
    , send
    , addServer
    , remServer
    
    , handleIrc
    , catchIrc
    
    , forkLB
    , liftLB
    
    , readConfig
    
    , debugStr
    , debugStrLn
    
    , withModule
    , withCommand
    , withAllModules
    , getDictKeys

    , verbose
    , proxy
    , ghci
    , outputDir
    , onStartupCmds
    , dynamicLoader
    ) where

import           Lambdabot.Command
import           Lambdabot.Config
import           Lambdabot.IRC (IrcMessage)
import           Lambdabot.Module
import qualified Lambdabot.Message as Msg
import qualified Lambdabot.Shared  as S
import           Lambdabot.Util.Signals
import           Lambdabot.Util

import Prelude hiding           (mod, catch)

import System.IO

import Data.Char
import Data.IORef               (newIORef, IORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M hiding (Map)

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Error (MonadError (..))
import Control.Monad.Reader
import Control.Monad.State

import System.Console.Haskeline.MonadException (MonadException)

-------------------------------------
-- Core configuration variables

configKey "verbose"         [t| Bool                    |] [| False         |]
configKey "proxy"           [t| Maybe ([Char], Integer) |] [| Nothing       |]
configKey "ghci"            [t| String                  |] [| "ghci"        |]
configKey "outputDir"       [t| FilePath                |] [| "State/"      |]
configKey "onStartupCmds"   [t| [String]                |] [| []            |]
configKey "dynamicLoader"   [t| Maybe S.DynLoad         |] [| Nothing       |]

------------------------------------------------------------------------
--
-- Lambdabot state
--

-- | Global read-only state.
data IRCRState = IRCRState
    { ircInitDoneMVar   :: MVar ()
    , ircQuitMVar       :: MVar ()
    , ircConfig         :: Config
    }

-- | Global read\/write state.
data IRCRWState = IRCRWState
    { ircServerMap       :: Map String (String, IrcMessage -> LB ())
    , ircPrivilegedUsers :: Map Msg.Nick Bool
    , ircIgnoredUsers    :: Map Msg.Nick Bool
    
    , ircChannels        :: Map ChanName String
    -- ^ maps channel names to topics
    
    , ircModules         :: Map String ModuleRef
    , ircCallbacks       :: Map String [(String,Callback)]
    , ircOutputFilters   :: [(String,OutputFilter)]
    -- ^ Output filters, invoked from right to left
    
    , ircCommands        :: Map String CommandRef
    , ircStayConnected   :: !Bool
    , ircPlugins         :: [String]
    }

type Callback = IrcMessage -> LB ()

type OutputFilter = Msg.Nick -> [String] -> LB [String]

newtype ChanName = ChanName Msg.Nick -- always lowercase
  deriving (Eq, Ord)

instance Show ChanName where show (ChanName x) = show x

data ModuleRef = forall st.
    ModuleRef (Module st) (MVar st) String

data CommandRef = forall st.
    CommandRef (Module st) (MVar st) (Command (ModuleT st LB)) String

-- | only use the "smart constructor":
mkCN :: Msg.Nick -> ChanName
mkCN = ChanName . liftM2 Msg.Nick Msg.nTag (map toLower . Msg.nName)

getCN :: ChanName -> Msg.Nick
getCN (ChanName n) = n

-- The virtual chat system.
--
-- The virtual chat system sits between the chat drivers and the rest of
-- Lambdabot.  It provides a mapping between the String server "tags" and
-- functions which are able to handle sending messages.
--
-- When a message is recieved, the chat module is expected to call
-- `Lambdabot.Main.received'.  This is not ideal.

addServer :: String -> (IrcMessage -> LB ()) -> ModuleT mod LB ()
addServer tag sendf = do
    s <- get
    let svrs = ircServerMap s
    name <- getModuleName
    case M.lookup tag svrs of
        Nothing -> put (s { ircServerMap = M.insert tag (name,sendf) svrs})
        Just _ -> fail $ "attempted to create two servers named " ++ tag

remServer :: String -> LB ()
remServer tag = do
    s <- get
    let svrs = ircServerMap s
    case M.lookup tag svrs of
        Just _ -> do
            let svrs' = M.delete tag svrs
            put (s { ircServerMap = svrs' })
            when (M.null svrs') $ do
                quitMVar <- asks ircQuitMVar
                io $ putMVar quitMVar ()
        Nothing -> fail $ "attempted to delete nonexistent servers named " ++ tag

send :: IrcMessage -> LB ()
send msg = do
    s <- gets ircServerMap
    case M.lookup (Msg.server msg) s of
        Just (_, sendf) -> sendf msg
        Nothing -> io $ hPutStrLn stderr $ "sending message to bogus server: " ++ show msg


-- ---------------------------------------------------------------------
--
-- The LB (LambdaBot) monad
--

-- | The IRC Monad. The reader transformer holds information about the
--   connection to the IRC server.
--
-- instances Monad, Functor, MonadIO, MonadState, MonadError


newtype LB a = LB { runLB :: ReaderT (IRCRState,IORef IRCRWState) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadException)

class (MonadIO m, Applicative m) => MonadLB m where
    lb :: LB a -> m a

instance MonadLB LB where lb = id

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
  throwError (SignalCaught e) = io $ evaluate (throw $ SignalException e)
  m `catchError` h = lbIO $ \conv -> (conv m
              `catch` \(SignalException e) -> conv $ h $ SignalCaught e)
              `catch` \e -> conv $ h $ IRCRaised e

-- A type for handling both Haskell exceptions and external signals
data IRCError = IRCRaised SomeException | SignalCaught Signal

instance Show IRCError where
    show (IRCRaised    e) = show e
    show (SignalCaught s) = show s

-- lbIO return :: LB (LB a -> IO a)
-- CPS to work around predicativiy of haskell's type system.
lbIO :: MonadLB m => ((forall a. LB a -> IO a) -> IO b) -> m b
lbIO k = lb $ do
    r <- LB ask
    io (k (flip runReaderT r . runLB))

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

-- | run an IO action in another thread, with a timeout, lifted into LB
forkLB :: LB a -> LB ThreadId
forkLB f = (`liftLB` f) $ \g -> do
            forkIO $ do
                timeout (15 * 1000 * 1000) g
                return ()

-- | lift an io transformer into LB
liftLB :: (IO a -> IO b) -> LB a -> LB b
liftLB f = LB . mapReaderT f . runLB -- lbIO (\conv -> f (conv lb))

readConfig :: MonadLB m => ConfigKey a -> m a
readConfig k = liftM (lookupConfig k) (lb (asks ircConfig))

-- | 'debugStr' checks if we have the verbose flag turned on. If we have
--   it outputs the String given. Else, it is a no-op.
debugStr :: MonadLB m => String -> m ()
debugStr str = do
    v <- readConfig verbose
    when v (io (putStr str))

-- | 'debugStrLn' is a version of 'debugStr' that adds a newline to the end
--   of the string outputted.
debugStrLn :: MonadLB m => String -> m ()
debugStrLn x = debugStr (x ++ "\n")

------------------------------------------------------------------------
-- Module handling

-- | Interpret an expression in the context of a module.
-- Arguments are which map to use (@ircModules@ and @ircCommands@ are
-- the only sensible arguments here), the name of the module\/command,
-- action for the case that the lookup fails, action if the lookup
-- succeeds.
--
withModule :: String
           -> LB a
           -> (forall st. Module st -> ModuleT st LB a)
           -> LB a

withModule modname def f = do
    maybemod <- gets (M.lookup modname . ircModules)
    case maybemod of
      -- TODO stick this ref stuff in a monad instead. more portable in
      -- the long run.
      Just (ModuleRef m ref name) -> do
          runReaderT (moduleT $ f m) (ref, name)
      _                           -> def

withCommand :: String
            -> LB a
            -> (forall st. Module st
                        -> Command (ModuleT st LB)
                        -> ModuleT st LB a)
            -> LB a

withCommand cmdname def f = do
    maybecmd <- gets (M.lookup cmdname . ircCommands)
    case maybecmd of
      -- TODO stick this ref stuff in a monad instead. more portable in
      -- the long run.
      Just (CommandRef m ref cmd name) -> do
          runReaderT (moduleT $ f m cmd) (ref, name)
      _                           -> def

-- | Interpret a function in the context of all modules
withAllModules :: (forall st. Module st -> ModuleT st LB a) -> LB [a]
withAllModules f = do
    mods <- gets $ M.elems . ircModules :: LB [ModuleRef]
    (`mapM` mods) $ \(ModuleRef m ref name) -> do
        runReaderT (moduleT $ f m) (ref, name)

getDictKeys :: (MonadState s m) => (s -> Map k a) -> m [k]
getDictKeys dict = gets (M.keys . dict)
