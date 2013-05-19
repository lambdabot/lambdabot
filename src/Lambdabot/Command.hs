{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Lambdabot.Command 
    ( Command(..)
    , cmdNames
    , command
    , runCommand
    , Cmd
    , execCmd
    , getCmdName
    , withMsg
    , readNick
    , showNick
    , getServer
    , getSender
    , getTarget
    , getLambdabotName
    , say
    ) where

import Lambdabot.Config
import Lambdabot.Logging
import qualified Lambdabot.Message as Msg
import Lambdabot.Nick

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Writer

data CmdArgs = forall a. Msg.Message a => CmdArgs
    { _message  :: a
    , target    :: Nick
    , invokedAs :: String
    }

newtype Cmd m a = Cmd { unCmd :: ReaderT CmdArgs (WriterT [String] m) a }
instance Functor f => Functor (Cmd f) where
    fmap f (Cmd x) = Cmd (fmap f x)
instance Applicative f => Applicative (Cmd f) where
    pure = Cmd . pure
    Cmd f <*> Cmd x = Cmd (f <*> x)
instance Monad m => Monad (Cmd m) where
    return = Cmd . return
    Cmd x >>= f = Cmd (x >>= (unCmd . f))
    fail = lift . fail
instance MonadIO m => MonadIO (Cmd m) where
    liftIO = lift . liftIO
instance MonadBase b m => MonadBase b (Cmd m) where
    liftBase = lift . liftBase
instance MonadTrans Cmd where
    lift = Cmd . lift . lift
instance MonadTransControl Cmd where
    newtype StT Cmd a = StCmd {unStCmd :: (a, [String])}
    liftWith f = do
        r <- Cmd ask
        lift $ f $ \t -> liftM StCmd (runWriterT (runReaderT (unCmd t) r))
    restoreT = Cmd . lift . WriterT . liftM unStCmd
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}
instance MonadBaseControl b m => MonadBaseControl b (Cmd m) where
    newtype StM (Cmd m) a = StMCmd {unStMCmd :: ComposeSt Cmd m a}
    liftBaseWith = defaultLiftBaseWith StMCmd
    restoreM     = defaultRestoreM     unStMCmd
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}
instance MonadConfig m => MonadConfig (Cmd m) where
    getConfig = lift . getConfig
instance MonadLogging m => MonadLogging (Cmd m) where
    getCurrentLogger = do
        parent <- lift getCurrentLogger
        self   <- getCmdName
        return (parent ++ ["Command", self])
    logM a b c = lift (logM a b c)

data Command m = Command
    { cmdName       :: String
    , aliases       :: [String]
    , privileged    :: Bool
    , help          :: Cmd m ()
    , process       :: String -> Cmd m ()
    }

cmdNames :: Command m -> [String]
cmdNames c = cmdName c : aliases c

command :: String -> Command Identity
command name = Command
    { cmdName       = name
    , aliases       = []
    , privileged    = False
    , help          = bug "they haven't created any help text!"
    , process       = const (bug "they haven't implemented this command!")
    } where
        bug reason = say $ unwords [ "You should bug the author of the", show name, "command, because", reason]

runCommand :: (Monad m, Msg.Message a) => Command m -> a -> Nick -> String -> String -> m [String]
runCommand cmd msg tgt arg0 args = execCmd (process cmd args) msg tgt arg0

execCmd ::  (Monad m, Msg.Message a) => Cmd m t -> a -> Nick -> String -> m [String]
execCmd cmd msg tgt arg0 = execWriterT (runReaderT (unCmd cmd) (CmdArgs msg tgt arg0))

getTarget :: Monad m => Cmd m Nick
getTarget = Cmd (asks target)

getCmdName :: Monad m => Cmd m String
getCmdName = Cmd (asks invokedAs)

say :: Monad m => String -> Cmd m ()
say [] = return ()
say it = Cmd (tell [it])

withMsg :: Monad m => (forall a. Msg.Message a => a -> Cmd m t) -> Cmd m t
withMsg f = Cmd ask >>= f'
    where f' (CmdArgs msg _ _) = f msg

readNick :: Monad m => String -> Cmd m Nick
readNick nick = withMsg (\msg -> return (parseNick (Msg.server msg) nick))

showNick :: Monad m => Nick -> Cmd m String
showNick nick = withMsg (\msg -> return (fmtNick (Msg.server msg) nick))

getServer :: Monad m => Cmd m String
getServer = withMsg (return . Msg.server)

getSender :: Monad m => Cmd m Nick
getSender = withMsg (return . Msg.nick)

getLambdabotName :: Monad m => Cmd m Nick
getLambdabotName = withMsg (return . Msg.lambdabotName)