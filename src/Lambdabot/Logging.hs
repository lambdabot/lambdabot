module Lambdabot.Logging
    ( L.Priority(..)
    , MonadLogging(..)
    , debugM
    , infoM
    , noticeM
    , warningM
    , errorM
    , criticalM
    , alertM
    , emergencyM
    ) where

import qualified System.Log.Logger as L

class Monad m => MonadLogging m where
    getCurrentLogger :: m String
    logM :: String -> L.Priority -> String -> m ()

instance MonadLogging IO where
    getCurrentLogger = return L.rootLoggerName
    logM = L.logM

debugM :: MonadLogging m => String -> m ()
debugM msg = do
    logger <- getCurrentLogger
    logM logger L.DEBUG msg

infoM :: MonadLogging m => String -> m ()
infoM msg = do
    logger <- getCurrentLogger
    logM logger L.INFO msg

noticeM :: MonadLogging m => String -> m ()
noticeM msg = do
    logger <- getCurrentLogger
    logM logger L.NOTICE msg

warningM :: MonadLogging m => String -> m ()
warningM msg = do
    logger <- getCurrentLogger
    logM logger L.WARNING msg

errorM :: MonadLogging m => String -> m ()
errorM msg = do
    logger <- getCurrentLogger
    logM logger L.ERROR msg

criticalM :: MonadLogging m => String -> m ()
criticalM msg = do
    logger <- getCurrentLogger
    logM logger L.CRITICAL msg

alertM :: MonadLogging m => String -> m ()
alertM msg = do
    logger <- getCurrentLogger
    logM logger L.ALERT msg

emergencyM :: MonadLogging m => String -> m ()
emergencyM msg = do
    logger <- getCurrentLogger
    logM logger L.EMERGENCY msg
