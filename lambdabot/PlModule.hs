
module PlModule (theModule) where

import PlModule.Common
import PlModule.Parser
import PlModule.PrettyPrinter
import PlModule.Transform

import Control.Concurrent
import Control.Concurrent.Chan

import Control.Monad.Trans

import GHC.Base

import IRC

-- firstTimeout is the timeout when the expression is simplified for the first
-- time. After each unsuccessful attempt, this number is doubled until it hits
-- maxTimeout.
firstTimeout, maxTimeout :: Int
firstTimeout =  3000000 --  3 seconds
maxTimeout   = 15000000 -- 15 seconds

type PlState = Maybe (Int, TopLevel)

newtype PlModule = PlModule ()

theModule :: MODULE
theModule = MODULE $ PlModule ()

type Pl m a = ModuleT PlState m a

instance Module PlModule PlState where
    moduleHelp _ "pl-resume" = return "@pl-resume - resume a suspended pointless transformation."
    moduleHelp _ _ = return "@pointless <expr> - play with pointfree code"

    moduleDefState _ = return Nothing

    moduleCmds _   = return ["pointless","pl-resume","pl"]

    process _ _ target "pointless" rest = pf target rest
    process _ _ target "pl"        rest = pf target rest
    process _ _ target "pl-resume" _ = res target
    process _ _ target _ _ =
      ircPrivmsg target "pointless: sorry, I don't understand."

res :: String -> Pl IRC ()
res target = do
  d <- readMS
  case d of
    Nothing -> ircPrivmsg target "pointless: sorry, nothing to resume."
    Just d' -> optimizeTopLevel target d'

pf :: String -> String -> Pl IRC ()
pf target inp = case parsePF inp of
  Right d -> optimizeTopLevel target (firstTimeout, mapTopLevel transform d)
  Left err -> ircPrivmsg target err

optimizeTopLevel :: String -> (Int, TopLevel) -> Pl IRC ()
optimizeTopLevel target (to, d) = do
  let (e,decl) = getExpr d
  (e', finished) <- liftIO $ optimizeIO to e
  ircPrivmsg target $ show $ decl e'
  if finished
    then writeMS Nothing
    else do
      ircPrivmsg target "optimization suspended, use @pl-resume to continue."
      writeMS $ Just (min (2*to) maxTimeout, decl e')

optimizeIO :: Int -> Expr -> IO (Expr, Bool)
optimizeIO to e = do
  chan <- newChan
  result <- timeout to $ writeList2Chan chan $ optimize e
  e' <- getChanLast chan e
  return $ case result of
    Nothing -> (e', False)
    Just _  -> (e', True)

getChanLast :: Chan a -> a -> IO a
getChanLast c x = do
  b <- isEmptyChan c
  if b then return x else getChanLast c =<< readChan c

-- stolen from
-- http://www.haskell.org/pipermail/haskell-cafe/2005-January/008314.html
parIO :: IO a -> IO a -> IO a
parIO a1 a2 = do
  m <- newEmptyMVar
  c1 <- forkIO $ putMVar m =<< a1
  c2 <- forkIO $ putMVar m =<< a2
  r <- takeMVar m
  killThread c1
  killThread c2
  return r

timeout :: Int -> IO a -> IO (Maybe a)
timeout n a = parIO (Just `fmap` a) (threadDelay n >> return Nothing)

