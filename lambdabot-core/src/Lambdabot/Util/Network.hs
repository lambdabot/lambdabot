{-# LANGUAGE CPP #-}

module Lambdabot.Util.Network (
    connectTo',
) where

import Network.Socket
import Network.BSD
import System.IO
import Control.Exception

-- |This is essentially a reimplementation of the former Network.connectTo
--  function, except that we don't do the service name lookup.

-- Code originally from the network package.
connectTo' :: HostName -> PortNumber -> IO Handle
connectTo' host port = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrProtocol = proto
                             , addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just (show port))
    firstSuccessful $ map tryToConnect addrs
  where
    tryToConnect addr =
      bracketOnError
          (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
          (close)  -- only done if there's an error
          (\sock -> do
            connect sock (addrAddress addr)
            socketToHandle sock ReadWriteMode
          )
    firstSuccessful = go []
      where
        go :: [IOException] -> [IO a] -> IO a
        go []      [] = ioError . userError $ "host name `" ++ show host ++
                        "` could not be resolved"
        go l@(_:_) [] = ioError . userError $ "could not connect to host `" ++
                        show host
        go acc     (act:followingActs) = do
            er <- try act
            case er of
                Left err -> go (err:acc) followingActs
                Right r  -> return r
