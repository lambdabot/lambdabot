{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
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
    , getSender
    , getTarget
    , getLambdabotName
    , say
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans
import Lambdabot.Message (Message, Nick)
import qualified Lambdabot.Message as Msg

data CmdArgs = forall a. Message a => CmdArgs
    { message   :: a
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
instance MonadTrans Cmd where
    lift = Cmd . lift . lift
instance MonadIO m => MonadIO (Cmd m) where
    liftIO = Cmd . liftIO

data Command m = Command
    { cmdName       :: String
    , aliases       :: [String]
    , privileged    :: Bool
    , help          :: Cmd m ()
    , process       :: String -> Cmd m ()
    }

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

runCommand :: (Monad m, Message a) => Command m -> a -> Nick -> String -> String -> m [String]
runCommand cmd msg tgt arg0 args = execCmd (process cmd args) msg tgt arg0

execCmd ::  (Monad m, Message a) => Cmd m t -> a -> Nick -> String -> m [String]
execCmd cmd msg tgt arg0 = execWriterT (runReaderT (unCmd cmd) (CmdArgs msg tgt arg0))

getTarget :: Monad m => Cmd m Nick
getTarget = Cmd (asks target)

getCmdName :: Monad m => Cmd m String
getCmdName = Cmd (asks invokedAs)

say :: Monad m => String -> Cmd m ()
say = Cmd . tell . return

withMsg :: Monad m => (forall a. Message a => a -> Cmd m t) -> Cmd m t
withMsg f = Cmd ask >>= f'
    where f' (CmdArgs msg _ _) = f msg

readNick :: Monad m => String -> Cmd m Nick
readNick nick = withMsg (return . flip Msg.readNick nick)

showNick :: Monad m => Nick -> Cmd m String
showNick nick = withMsg (return . flip Msg.showNick nick)

getSender :: Monad m => Cmd m Nick
getSender = withMsg (return . Msg.nick)

getLambdabotName :: Monad m => Cmd m Nick
getLambdabotName = withMsg (return . Msg.lambdabotName)