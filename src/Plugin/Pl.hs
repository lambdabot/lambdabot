-- | Pointfree programming fun
--
-- A catalogue of refactorings is at:
--      http://www.cs.kent.ac.uk/projects/refactor-fp/catalogue/
--      http://www.cs.kent.ac.uk/projects/refactor-fp/catalogue/RefacIdeasAug03.html
--
-- Use more Arrow stuff
--
-- TODO would be to plug into HaRe and use some of their refactorings.
module Plugin.Pl (theModule) where

import Plugin

import Plugin.Pl.Common          (TopLevel, mapTopLevel, getExpr)
import Plugin.Pl.Parser          (parsePF)
import Plugin.Pl.PrettyPrinter   (Expr)
import Plugin.Pl.Transform       (transform)
import Plugin.Pl.Optimize        (optimize)

import Control.Concurrent.Chan    (Chan, newChan, isEmptyChan, readChan, writeList2Chan)
import Control.Exception (unblock)
-- firstTimeout is the timeout when the expression is simplified for the first
-- time. After each unsuccessful attempt, this number is doubled until it hits
-- maxTimeout.
firstTimeout, maxTimeout :: Int
firstTimeout =  3000000 --  3 seconds
maxTimeout   = 15000000 -- 15 seconds

type PlState = GlobalPrivate () (Int, TopLevel)
type Pl = ModuleT PlState LB

theModule = newModule
    { moduleDefState = return $ mkGlobalPrivate 15 ()
    
    , moduleCmds = return
        [ (command "pointless")
            { aliases = ["pl"]
            , help = say "pointless <expr>. Play with pointfree code."
            , process = pf
            }
        , (command "pl-resume")
            { help = say "pl-resume. Resume a suspended pointless transformation."
            , process = const res
            }
        ]
    }

------------------------------------------------------------------------

res :: Cmd Pl ()
res = do
  d <- readPS =<< getTarget
  case d of
    Just d' -> optimizeTopLevel d'
    Nothing -> say "pointless: sorry, nothing to resume."

-- | Convert a string to pointfree form
pf :: String -> Cmd Pl ()
pf inp = do
    case parsePF inp of
        Right d  -> optimizeTopLevel (firstTimeout, mapTopLevel transform d)
        Left err -> say err

optimizeTopLevel :: (Int, TopLevel) -> Cmd Pl ()
optimizeTopLevel (to, d) = do
    target <- getTarget
    let (e,decl) = getExpr d
    (e', finished) <- io $ optimizeIO to e
    let eDecl = decl e'
    say (show eDecl)
    if finished
        then writePS target Nothing
        else do
            writePS target $ Just (min (2*to) maxTimeout, eDecl)
            say "optimization suspended, use @pl-resume to continue."

------------------------------------------------------------------------

optimizeIO :: Int -> Expr -> IO (Expr, Bool)
optimizeIO to e = do
  chan <- newChan
  -- We need "unblock" here because "timeout" will terminate the thread
  -- with an async exception and the current thread is in blocked
  -- mode for reasons that still aren't clear to me.
  result <- timeout to $ unblock (writeList2Chan chan $ optimize e)
  e' <- getChanLast chan e
  return $ case result of
    Nothing -> (e', False)
    Just _  -> (e', True)

getChanLast :: Chan a -> a -> IO a
getChanLast c x = do
  b <- isEmptyChan c
  if b then return x else getChanLast c =<< readChan c
