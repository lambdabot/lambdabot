--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module Main(main) where
import Data.Char(isAlpha, isSpace, isAlphaNum)
import Data.List(sortBy, nub, intersperse)
import Data.Ratio
import Text.ParserCombinators.ReadP
import Control.Monad(when)
import Control.Monad.Error()
import System.IO
import System.Environment
import System.Exit

import REPL
import LJT
--import MJ
import HTypes
import HCheck(htCheckEnv, htCheckType)
import Help

main :: IO ()
main = do
    args <- getArgs
    let decodeOptions (('-':cs) : as) st = decodeOption cs >>= \f -> decodeOptions as (f False st)
        decodeOptions (('+':cs) : as) st = decodeOption cs >>= \f -> decodeOptions as (f True  st)
        decodeOptions as st = return (as, st)
        decodeOption cs = case [ set | (cmd, _, _, set) <- options, isPrefix cs cmd ] of
                          [] -> do usage; exitWith (ExitFailure 1)
                          set : _ -> return set
    (args', state) <- decodeOptions args startState
    case args' of
        [] -> repl (hsGenRepl state)
        _ -> loop state args'
              where loop _ [] = return ()
                    loop s (a:as) = do
                        putStrLn $ "-- loading file " ++ a
                        (q, s') <- loadFile s a
                        if q then
                            return ()
                         else
                            loop s' as

usage :: IO ()
usage = putStrLn "Usage: djinn [option ...] [file ...]"

hsGenRepl :: State -> REPL State
hsGenRepl state = REPL {
    repl_init = inIt state,
    repl_eval = eval,
    repl_exit = exit
    }

data State = State {
    synonyms :: [(HSymbol, ([HSymbol], HType, HKind))],
    axioms :: [(HSymbol, HType)],
    classes :: [ClassDef],
    multi :: Bool,
    sorted :: Bool,
    debug :: Bool,
    cutOff :: Int
    }
    deriving (Show)

startState :: State
startState = State {
    synonyms = syns,
    classes = clss,
    axioms = [],
    multi = False,
    sorted = True,
    debug = False,
    cutOff = 100
    }
 where syns = either (const $ error "Bad initial environment") id $ htCheckEnv $ reverse [
        ("()",     ([],        HTUnion [("()",[])],                                      undefined)),
        ("Either", (["a","b"], HTUnion [("Left", [HTVar "a"]), ("Right", [HTVar "b"])],  undefined)),
        ("Maybe",  (["a"],     HTUnion [("Nothing", []), ("Just", [HTVar "a"])],         undefined)),
        ("Bool",   ([],        HTUnion [("False", []), ("True", [])],                    undefined)),
        ("Void",   ([],        HTUnion [],                                               undefined)),
        ("Not",    (["x"],     htNot "x",                                                undefined))
        ]
       clss = [("Eq", (["a"], [("==", a `HTArrow` (a `HTArrow` HTCon "Bool"))]))]
       a = HTVar "a"


version :: String
version = "version 2007-04-20"

inIt :: State -> IO (String, State)
inIt state = do
    putStrLn $ "Welcome to Djinn " ++ version ++ "."
    putStrLn $ "Type :h to get help."
    return ("Djinn> ", state)

eval :: State -> String -> IO (Bool, State)
eval s line =
    case filter (null . snd) (readP_to_S pCmd line) of
    [] -> do
                putStrLn $ "Cannot parse command"
                return (False, s)
    (cmd, "") : _ -> runCmd s cmd
    _ -> error "eval"

exit :: State -> IO ()
exit _s = do
    putStrLn "Bye."
    return ()

type Context = (HSymbol, [HType])
type ClassDef = (HSymbol, ([HSymbol], [Method]))

data Cmd = Help Bool | Quit | Add HSymbol HType | Query HSymbol [Context] HType | Del HSymbol | Load HSymbol | Noop | Env |
           Type (HSymbol, ([HSymbol], HType, HKind)) | Set (State -> State) | Clear | Class ClassDef

pCmd :: ReadP Cmd
pCmd = do
    skipSpaces
    let adds (':':s) p = do schar ':'; pPrefix (takeWhile (/= ' ') s); c <- p; skipSpaces; return c
        adds _ p = do c <- p; skipSpaces; return c
    cmd <- foldr1 (+++) [ adds s p | (s, _, p) <- commands ]
    skipSpaces
    return cmd

pPrefix :: String -> ReadP String
pPrefix s = do
    skipSpaces
    cs <- look
    let w = takeWhile isAlpha cs
    if isPrefix w s then
        string w
     else
        pfail

isPrefix :: String -> String -> Bool
isPrefix p s = not (null p) && length p <= length s && take (length p) s == p

runCmd :: State -> Cmd -> IO (Bool, State)
runCmd s Noop = return (False, s)
runCmd s (Help verbose) = do
    putStr $ helpText ++ unlines (map getHelp commands) ++ getSettings s
    when verbose $ putStr verboseHelp
    return (False, s)
runCmd s Quit = 
    return (True, s)
runCmd s (Load f) = loadFile s f
runCmd s (Add i t) = 
    case htCheckType (synonyms s) t of
    Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
    Right _ -> return (False, s { axioms = (i, t) : axioms s })
runCmd _ Clear =
    return (False, startState)
runCmd s (Del i) = 
    return (False, s { axioms   = filter ((i /=) . fst) (axioms s)
                     , synonyms = filter ((i /=) . fst) (synonyms s)
                     , classes = filter ((i /=) . fst) (classes s) })
runCmd s Env = do
--    print s
    let tname t = if isHTUnion t then "data" else "type"
        showd (HTUnion []) = ""
        showd t = " = " ++ show t
    mapM_ (\ (i, (vs, t, _)) -> putStrLn $ tname t ++ " " ++ unwords (i:vs) ++ showd t) (reverse $ synonyms s)
    mapM_ (\ (i, t) -> putStrLn $ i ++ " :: " ++ show t) (reverse $ axioms s)
    mapM_ (putStrLn . showClass) (reverse $ classes s)
    return (False, s)
runCmd s (Type syn) = do
    case htCheckEnv (syn : synonyms s) of
        Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
        Right syns -> return (False, s { synonyms = syns })
runCmd s (Set f) =
    return (False, f s)
runCmd s (Query i ctx g) =
   case htCheckType (synonyms s) g >> mapM (ctxLookup (classes s)) ctx of
   Left msg -> do putStrLn $ "Error: " ++ msg; return (False, s)
   Right mss -> do
    let form = hTypeToFormula (synonyms s) g
        env = [ (Symbol v, hTypeToFormula (synonyms s) t) | (v, t) <- axioms s ] ++ ctxEnv
        ctxEnv = [ (Symbol v, hTypeToFormula (synonyms s) t) | ms <- mss, (v, t) <- ms ]
        mpr = prove (multi s || sorted s) env form
    when (debug s) $ putStrLn ("*** " ++ show form)
    case mpr of
        [] -> do
            putStrLn $ "-- " ++ i ++ " cannot be realized."
            return (False, s)
        ps -> do
            let f p =
                   let c = termToHClause i p
                       bvs = getBinderVars c
                       r = if null bvs then (0, 0) else (length (filter (== "_") bvs) % length bvs, length bvs)
                   in  (r, c)
                e:es = nub $ 
                        if sorted s then
                            map snd $ sortBy (\ (x,_) (y,_) -> compare x y) $ map f $ take (cutOff s) ps
                        else
                            map (termToHClause i) $ take (cutOff s) ps
                pr = putStrLn . hPrClause
                sctx = if null ctx then "" else showContexts ctx ++ " => "
            when (debug s) $ putStrLn ("+++ " ++ show (head ps))
            putStrLn $ i ++ " :: " ++ sctx ++ show g
            pr e
            when (multi s) $ mapM_ (\ x -> putStrLn "-- or" >> pr x) es
            return (False, s)
runCmd s (Class c) = do
    return (False, s { classes = c : classes s })

loadFile :: State -> String -> IO (Bool, State)
loadFile s name = do
    file <- readFile name
    evalCmds s $ lines $ stripComments file

stripComments :: String -> String
stripComments "" = ""
stripComments ('-':'-':cs) = skip cs
  where skip "" = ""
        skip s@('\n':_) = stripComments s
        skip (_:s) = skip s
stripComments (c:cs) = c : stripComments cs

showClass :: ClassDef -> String
showClass (c, (as, ms)) = "class " ++ showContext (c, map HTVar as) ++ " where " ++ concat (intersperse "; " $ map sm ms)
  where sm (i, t) = pp i ++ " :: " ++ show t
        pp i@(ch:_) | not (isAlphaNum ch) = "(" ++ i ++ ")"
        pp i = i

showContext :: Context -> String
showContext (c, as) = show $ foldl HTApp (HTCon c) as

showContexts :: [Context] -> String
showContexts [] = ""
showContexts cs = "(" ++ concat (intersperse ", " $ map showContext cs) ++ ")"

ctxLookup :: [ClassDef] -> Context -> Either String [Method]
ctxLookup clss (c, as) =
    case lookup c clss of
    Nothing -> Left $ "Class not found: " ++ c
    Just (ps, ms) -> Right [(m, substHT (zip ps as) t) | (m, t) <- ms ]

evalCmds :: State -> [String] -> IO (Bool, State)
evalCmds state [] = return (False, state)
evalCmds state (l:ls) = do
    qs@(q, state') <- eval state l
    if q then
        return qs
     else
        evalCmds state' ls

commands :: [(String, String, ReadP Cmd)]
commands = [
        (":clear",              "Clear the envirnment",         return Clear),
        (":delete <sym>",       "Delete from environment.",     pDel),
        (":environment",        "Show environment",             return Env),
        (":help",               "Print this message.",          return (Help False)),
        (":load <file>",        "Load a file",                  pLoad),
        (":quit",               "Quit program.",                return Quit),
        (":set <option>",       "Set options",                  pSet),
        (":verbose-help",       "Print verbose help.",          return (Help True)),
        ("type <sym> <vars> = <type>", "Add a type synonym",    pType),
        ("data <sym> <vars> = <datatype>", "Add a data type",   pData),
        ("class <sym> <vars> where <method>...", "Add a class", pClass),
        ("<sym> :: <type>",     "Add to environment",           pAdd),
        ("<sym> ? <type>",      "Query",                        pQuery),
        ("",                    "",                             return Noop)
        ]

options :: [(String, String, State->Bool, Bool->State->State)]
options = [
          ("multi",             "print multiple solutions",     multi,  \ v s -> s { multi  = v }),
          ("sorted",            "sort solutions",               sorted, \ v s -> s { sorted = v }),
          ("debug",             "debug mode",                   debug,  \ v s -> s { debug  = v })
          ]

getHelp :: (String, String, a) -> String
getHelp (cmd, help, _) = cmd ++ replicate (35 - length cmd) ' ' ++ help

pDel :: ReadP Cmd
pDel = do
    s <- pHSymbol True +++ pHSymbol False
    return $ Del s

pLoad :: ReadP Cmd
pLoad = do
    skipSpaces
    s <- munch1 (not . isSpace)
    return $ Load s

pAdd :: ReadP Cmd
pAdd = do
    i <- pHSymbol False
    sstring "::"
    t <- pHType
    optional $ schar ';'
    return $ Add i t

pQuery :: ReadP Cmd
pQuery = do
    i <- pHSymbol False
    schar '?'
    c <- option [] pContext
    t <- pHType
    optional $ schar ';'
    return $ Query i c t

pContext :: ReadP [Context]
pContext = do
    let pCtx = do c <- pHSymbol True; ts <- many pHTAtom; return (c, ts)
    schar '('
    ctx <- sepBy1 pCtx (schar ',')
    schar ')'
    sstring "=>"
    return ctx

pType :: ReadP Cmd
pType = do
    sstring "type"
    syn <- pHSymbol True
    args <- many (pHSymbol False)
    schar '='
    t <- pHType
    return $ Type (syn, (args, t, undefined))

pData :: ReadP Cmd
pData = do
    sstring "data"
    syn <- pHSymbol True
    args <- many (pHSymbol False)
    (do schar '='; t <- pHDataType; return $ Type (syn, (args, t, undefined))) +++ (return $ Type (syn, (args, HTUnion [], undefined)))

pClass :: ReadP Cmd
pClass = do
    sstring "class"
    cls <- pHSymbol True
    args <- many (pHSymbol False)
    sstring "where"
    mets <- sepBy pMethod (schar ';')
    return $ Class (cls, (args, mets))

type Method = (HSymbol, HType)

pMethod :: ReadP Method
pMethod = do
    let pOpSym = satisfy (`elem` "~!#$%^&*-+=<>.:")
    i <- pHSymbol False +++ do schar '('; op <- many1 pOpSym; schar ')'; return op
    sstring "::"
    t <- pHType
    return (i, t)

pSet :: ReadP Cmd
pSet = do
    val <- (do schar '+'; return True) +++ (do schar '-'; return False) 
    f <- foldr (+++) pfail [ do pPrefix s; return (set val) | (s, _, _, set) <- options ]
    return $ Set $ f

schar :: Char -> ReadP ()
schar c = do
    skipSpaces
    char c
    return ()

sstring :: String -> ReadP ()
sstring s = do
    skipSpaces
    string s
    return ()

helpText :: String
helpText = "\
\Djinn is a program that generates Haskell code from a type.\n\
\Given a type the program will deduce an expression of this type,\n\
\if one exists.  If the Djinn says the type is not realizable it is\n\
\because there is no (total) expression of the given type.\n\
\Djinn only knows about tuples, ->, and some data types in the\n\
\initial environment (do :e for a list).\n\
\\n\
\Caveat emptor: The expression will have the right type, but it\n\
\may not be what you were looking for.\n\
\\n\
\Send any comments and feedback to lennart@augustsson.net\n\
\\n\
\Commands (may be abbreviated):\n\
\"

getSettings :: State -> String
getSettings s = unlines $ [
    "",
    "Current settings" ] ++ [ "    " ++ (if gett s then "+" else "-") ++ name ++ replicate (10 - length name) ' ' ++ descr |
                              (name, descr, gett, _set) <- options ]
