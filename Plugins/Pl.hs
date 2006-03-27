{-# OPTIONS -fvia-C -O2 -optc-O3 #-}
-- ^ required to get results. -fasm seems to slow(!)
--
-- | Pointfree programming fun
--
-- A catalogue of refactorings is at:
--      http://www.cs.kent.ac.uk/projects/refactor-fp/catalogue/
--      http://www.cs.kent.ac.uk/projects/refactor-fp/catalogue/RefacIdeasAug03.html
--
-- Use more Arrow stuff
--
-- TODO would be to plug into HaRe and use some of their refactorings.
--
module Plugins.Pl (theModule) where

import Lambdabot
import LBState
import Util                       (timeout)

import Plugins.Pl.Common          (TopLevel, mapTopLevel, getExpr)
import Plugins.Pl.Parser          (parsePF)
import Plugins.Pl.PrettyPrinter   (Expr)
import Plugins.Pl.Transform       (transform, optimize)

import Control.Concurrent.Chan    (Chan, newChan, isEmptyChan, readChan, writeList2Chan)

import Control.Monad.Trans        (liftIO)

-- firstTimeout is the timeout when the expression is simplified for the first
-- time. After each unsuccessful attempt, this number is doubled until it hits
-- maxTimeout.
firstTimeout, maxTimeout :: Int
firstTimeout =  3000000 --  3 seconds
maxTimeout   = 15000000 -- 15 seconds

type PlState = GlobalPrivate () (Int, TopLevel)

newtype PlModule = PlModule ()

theModule :: MODULE
theModule = MODULE $ PlModule ()

type Pl = ModuleLB PlState

instance Module PlModule PlState where

    moduleCmds _   = ["pointless","pl-resume","pl"]

    moduleHelp _ "pl-resume" = "pl-resume. Resume a suspended pointless transformation."
    moduleHelp _ _           = "pointless <expr>. Play with pointfree code."

    moduleDefState _ = return $ mkGlobalPrivate 15 ()

    process _ _ target "pointless" rest = pf target rest
    process _ _ target "pl"        rest = pf target rest
    process _ _ target "pl-resume" _    = res target

------------------------------------------------------------------------

res :: String -> Pl
res target = do
  d <- readPS target
  case d of
    Nothing -> return ["pointless: sorry, nothing to resume."]
    Just d' -> optimizeTopLevel target d'

pf :: String -> String -> Pl
pf target inp = case parsePF inp of
  Right d  -> optimizeTopLevel target (firstTimeout, mapTopLevel transform d)
  Left err -> return [err]

optimizeTopLevel :: String -> (Int, TopLevel) -> Pl
optimizeTopLevel target (to, d) = do
  let (e,decl) = getExpr d
  (e', finished) <- liftIO $ optimizeIO to e
  extra <- if finished
           then do writePS target Nothing
                   return []
           else do writePS target $ Just (min (2*to) maxTimeout, decl e')
                   return ["optimization suspended, use @pl-resume to continue."]
  return $ (show $ decl e') : extra

------------------------------------------------------------------------

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
