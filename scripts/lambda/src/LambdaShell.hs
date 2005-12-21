{-
 -   The Lambda Shell, and interactive environment for evaluating pure untyped lambda terms.
 -   Copyright (C) 2005, Robert Dockins
 -
 -   This program is free software; you can redistribute it and/or modify
 -   it under the terms of the GNU General Public License as published by
 -   the Free Software Foundation; either version 2 of the License, or
 -   (at your option) any later version.
 -
 -   This program is distributed in the hope that it will be useful,
 -   but WITHOUT ANY WARRANTY; without even the implied warranty of
 -   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -   GNU General Public License for more details.
 -
 -   You should have received a copy of the GNU General Public License
 -   along with this program; if not, write to the Free Software
 -   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 -}


module LambdaShell
( LambdaShellState (..)
, initialShellState
, lambdaShell
, readDefinitionFile
, RS
)
where

import System.IO
import Data.List (isPrefixOf)

import Lambda
import LambdaParser
import Version

import qualified Data.Map as Map
import System.Console.Shell
import Text.ParserCombinators.Parsec (parse)

type RS = ReductionStrategy () String


-------------------------------------------------------
-- Define types to allow completion of let-bound names

completeLetBindings :: LambdaShellState -> String -> IO [String]
completeLetBindings st prefix =
    return 
    $ filter (prefix `isPrefixOf`)
    $ Map.keys
    $ letBindings st

data LetBinding = LetBinding

instance Completion LetBinding LambdaShellState where
  complete _         = completeLetBindings
  completableLabel _ = "<name>"



----------------------------------------------------------
-- Define the shell state

-- | Keeps track of all the state that is needed for the
--   operation of the lambda shell.

data LambdaShellState =
  LambdaShellState
  { trace       :: Bool      -- ^ Step through the reduction one redex at a time
  , traceNum    :: Int       -- ^ Number of reduction steps to display during tracing
  , letBindings :: Map.Map String (PureLambda () String)
                             -- ^ All \"let\" bindings currently in scope
  , fullUnfold  :: Bool      -- ^ Should binding names be eagerly unfolded?
  , redStrategy :: RS        -- ^ The reduction strategy currently in use
  , showCount   :: Bool
  }


-- | Default settings for all elements of shell state.
initialShellState =
  LambdaShellState
  { trace       = False
  , traceNum    = 10
  , letBindings = Map.empty
  , fullUnfold  = False
  , redStrategy = lamReduceWHNF
  , showCount   = False
  }



-----------------------------------------------------------------
-- Main entry point to the shell

-- | Run an interactive shell starting with the
--   given shell state and returning the final,
--   possibly modified, state.
lambdaShell :: LambdaShellState -> IO LambdaShellState
lambdaShell init = do
    desc <- mkShellDescription commands evaluate
    let desc' = desc{ defaultCompletions = Just completeLetBindings }
    runShell desc' init



-----------------------------------------------------------------
-- read definitions from a file

readDefinitionFile :: Bindings () String -> String -> IO (Bindings () String)
readDefinitionFile b file = do
  str  <- openFile file ReadMode >>= hGetContents
  let str' = stripComments str
  case parse (definitionFileParser b) file str' of
        Left err -> fail (show err)
        Right b' -> return b'



----------------------------------------------------------------
-- Definition of all the shell commands

commands :: [ShellCommand LambdaShellState]
commands =
  [ exitCommand "quit"
  , exitCommand "exit"
  , helpCommand "help"
  , cmd "trace"      toggleTrace     "Toggles the trace mode"
  , cmd "traceStep"  setTraceStep    "Sets the number of steps shown in trace mode"
  , cmd "dumpTrace"  dumpTrace       "Dumps a trace of the named term into a file"
  , cmd "unfold"     toggleUnfold    "Toggles the full unfold mode"
  , cmd "showall"    showBindings    "Shows all let bindings"
  , cmd "show"       showBinding     "Show a let binding"
  , cmd "whnf"       setRedWHNF      "Set reduction strategy to weak head normal form"
  , cmd "hnf"        setRedHNF       "Set reduction strategy to head normal form"
  , cmd "nf"         setRedNF        "Set reduction strategy to normal form"
  , cmd "strict"     setRedStrict    "Use applicative order (strict) reduction"
  , cmd "nowarranty" printNoWarranty "Print the warranty disclaimer"
  , cmd "gpl"        printGPL        "Print the GNU GPLv2, under which this software is licensed"
  , cmd "version"    printVersion    "Print version info"
  , cmd "load"       loadDefFile     "Load definitions from a file"
  , cmd "clear"      clearBindings   "Clear all let bindings"
  , cmd "showCount"  toggleShowCount "Toggle the show count mode"
  ]
  
toggleTrace :: StateCommand LambdaShellState
toggleTrace = StateCommand $ \st -> do
   if trace st 
      then putStrLn "trace off" >> return st{ trace = False }
      else putStrLn "trace on"  >> return st{ trace = True }

toggleUnfold :: StateCommand LambdaShellState
toggleUnfold = StateCommand $ \st -> do
   if fullUnfold st
      then putStrLn "full unfold off" >> return st{ fullUnfold = False }
      else putStrLn "full unfold on"  >> return st{ fullUnfold = True }

toggleShowCount :: StateCommand LambdaShellState
toggleShowCount = StateCommand $ \st -> do
   if showCount st
      then putStrLn "show count off"  >> return st{ showCount = False }
      else putStrLn "show count on"   >> return st{ showCount = True }


dumpTrace :: File -> Int -> Completable LetBinding -> StateCommand LambdaShellState
dumpTrace (File f) steps (Completable termStr) = StateCommand $ \st -> do

   case parse (lambdaParser (letBindings st)) "" termStr of
      Left msg   -> putStrLn (show msg)
      Right term -> do
         let trace = lamEvalTrace (letBindings st) (fullUnfold st) 
                                  (redStrategy st)
                                  (unfoldTop (letBindings st) term)
         h <- openFile f WriteMode
         hPutStr h $ unlines $ map printLam $ take steps $ trace
         hClose h
   return st

setTraceStep :: Int -> StateCommand LambdaShellState
setTraceStep step = StateCommand $ \st -> return st{ traceNum = step }

showBinding :: Completable LetBinding -> StateCommand LambdaShellState
showBinding (Completable name) = StateCommand $ \st -> do
    case Map.lookup name (letBindings st) of
        Nothing -> putStrLn $ concat ["'",name,"' not bound"]
        Just t  -> putStrLn $ concat [name," = ",printLam t]
    return st

showBindings :: StateCommand LambdaShellState
showBindings = StateCommand $ \st -> do
   putStrLn $
     Map.foldWithKey
       (\name t x -> concat [name," = ",printLam t,"\n",x])
       ""
       (letBindings st)
   return st

clearBindings :: StateCommand LambdaShellState
clearBindings = StateCommand $ \st -> return st{ letBindings = Map.empty }

loadDefFile :: File -> StateCommand LambdaShellState
loadDefFile (File path) = StateCommand $ \st -> do
   newBinds <- readDefinitionFile (letBindings st) path
   return st{ letBindings = newBinds }   

setRedWHNF :: StateCommand LambdaShellState
setRedWHNF = setRed lamReduceWHNF "weak head normal form"

setRedHNF :: StateCommand LambdaShellState
setRedHNF = setRed lamReduceHNF "head normal form"

setRedNF :: StateCommand LambdaShellState
setRedNF = setRed lamReduceNF "normal form"

setRedStrict :: StateCommand LambdaShellState
setRedStrict = setRed lamStrictNF "applicative order"

setRed :: RS -> String -> StateCommand LambdaShellState
setRed strategy name = StateCommand $ \st -> do
  putStrLn ("using reduction strategy: "++name)
  return st{ redStrategy = strategy }

printNoWarranty :: SimpleCommand LambdaShellState
printNoWarranty = SimpleCommand (putStr noWarranty)

printGPL :: SimpleCommand LambdaShellState
printGPL = SimpleCommand (putStrLn gpl)

printVersion :: SimpleCommand LambdaShellState
printVersion = SimpleCommand (putStrLn versionInfo)

----------------------------------------------------------------
-- Normal statement evaluation

evaluate :: EvaluationFunction LambdaShellState
evaluate str st = do
  case parse (statementParser (letBindings st)) "" str of
     Left err   -> putStrLn (show err) >> return (Right st)
     Right stmt -> evalStmt stmt st    >>= return . Right



evalStmt :: Statement -> LambdaShellState -> IO LambdaShellState
evalStmt (Stmt_eval expr) st     = evalExpr expr st
evalStmt (Stmt_isEq x y) st      = compareExpr x y st
evalStmt (Stmt_let name expr) st = return st{ letBindings = Map.insert name expr (letBindings st) }
evalStmt (Stmt_empty) st         = return st



evalExpr :: PureLambda () String -> LambdaShellState -> IO LambdaShellState

evalExpr t st = doEval (unfoldTop (letBindings st) t)

 where doEval  x = if trace st
                      then traceEval x st
                      else if showCount st
                              then evalCount x
                              else eval x

       evalCount t = do
            let (z,n) = lamEvalCount (letBindings st) (fullUnfold st) (redStrategy st) t
	    putStrLn (printLam z)
            putStrLn $ concat [show n," reductions"]
            return st

       eval t = do 
            let z = lamEval (letBindings st) (fullUnfold st) (redStrategy st) t
            putStrLn (printLam z)
	    return st



traceEval :: PureLambda () String -> LambdaShellState -> IO LambdaShellState

traceEval term st = do
  subShell <- traceSubshell term
  runSubshell subShell st




compareExpr :: PureLambda () String 
            -> PureLambda () String
	    -> LambdaShellState 
	    -> IO LambdaShellState

compareExpr x y st = do
     if normalEq (letBindings st) x y
        then putStrLn "equal"
        else putStrLn "not equal"
     return st



----------------------------------------------------------------
-- All the stuff for the tracing subshell

data TraceShellState 
   = TraceShellState
     { tracePos  :: Int
     , traceStep :: Int
     , traceList :: [PureLambda () String]
     }

mkTraceDesc :: IO (ShellDescription TraceShellState)
mkTraceDesc = do
  desc <- initialShellDescription
  return desc{ prompt        = "  ]"
             , commandStyle  = OnlyCommands
	     , shellCommands = traceShellCommands
             , beforePrompt  = printTrace
             }

traceShellCommands :: [ShellCommand TraceShellState]
traceShellCommands =
  [ cmd "p" tracePrev "previous"
  , cmd "n" traceNext "next"
  , exitCommand "q"
  ]

printTrace :: TraceShellState -> IO ()
printTrace st = do
  putStr $ unlines $ map (\(n,t) -> concat[show n,") ",printLam t]) $
	take (traceStep st) $ drop (tracePos st) $ zip [1..] (traceList st)

tracePrev :: StateCommand TraceShellState
tracePrev = StateCommand $ \st -> do
  let x = max 0 (tracePos st - traceStep st)
  return st{ tracePos = x}

traceNext :: StateCommand TraceShellState
traceNext = StateCommand $ \st -> do
  let x = tracePos st + traceStep st
  if null (drop (tracePos st) (traceList st))
     then return st
     else return st{ tracePos = x }

mkTraceState :: PureLambda () String -> LambdaShellState -> IO TraceShellState
mkTraceState term st =
   return TraceShellState
          { tracePos = 0
          , traceStep = traceNum st
          , traceList = lamEvalTrace (letBindings st) (fullUnfold st) (redStrategy st) term
          }

traceSubshell :: PureLambda () String -> IO (Subshell LambdaShellState TraceShellState)
traceSubshell term = do
  desc <- mkTraceDesc
  simpleSubshell (mkTraceState term) desc
