module Main(main) where
import Char(isAlpha)
import Text.ParserCombinators.ReadP
import Monad(when)
import IO
import System

import REPL
import LJT
import HTypes
import Help

main :: IO ()
main = do
    args <- getArgs
    case args of
	('-':_) : _ -> do usage; exitWith (ExitFailure 1)
	[] -> repl hsGenRepl
	_ -> loop startState args
	      where loop _ [] = return ()
		    loop s (a:as) = do
		        (q, s') <- loadFile s a
			if q then
			    return ()
			 else
			    loop s' as

usage :: IO ()
usage = putStrLn "Usage: djinn [file ...]"

hsGenRepl :: REPL State
hsGenRepl = REPL {
    repl_init = inIt,
    repl_eval = eval,
    repl_exit = exit
    }

data State = State {
    synonyms :: [(HSymbol, ([HSymbol], HType))],
    axioms :: [(HSymbol, HType)],
    multi :: Bool
    }
startState :: State
startState = State {
    synonyms = [("Not", (["x"], htNot "x"))],
    axioms = [],
    multi = False
    }

version :: String
version = "version 2005-12-12"

inIt :: IO (String, State)
inIt = do
    putStrLn $ "Welcome to Djinn " ++ version ++ "."
    putStrLn $ "Type :h to get help."
    return ("Djinn> ", startState)

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

data Cmd = Help Bool | Quit | Add HSymbol HType | Query HSymbol HType | Del HSymbol | Load HSymbol | Noop | Env |
	   Type (HSymbol, ([HSymbol], HType)) | Set (State -> State)

pCmd :: ReadP Cmd
pCmd = do
    skipSpaces
    let adds (':':s) p = do schar ':'; pPrefix s; c <- p; skipSpaces; return c
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
isPrefix p s = length p <= length s && take (length p) s == p

runCmd :: State -> Cmd -> IO (Bool, State)
runCmd s Noop = return (False, s)
runCmd s (Help verbose) = do
    putStr $ helpText ++ unlines (map getHelp commands)
    when verbose $ putStr verboseHelp
    return (False, s)
runCmd s Quit = 
    return (True, s)
runCmd s (Load f) = loadFile s f
runCmd s (Add i t) = 
    return (False, s { axioms = (i, t) : axioms s })
runCmd s (Del i) = 
    return (False, s { axioms = [ (i', t) | (i', t) <- axioms s, i /= i' ] })
runCmd s Env = do
    mapM_ (\ (i, (vs, t)) -> putStrLn $ "type " ++ unwords (i:vs) ++ " = " ++ show t) (synonyms s)
    mapM_ (\ (i, t) -> putStrLn $ i ++ " :: " ++ show t) (axioms s)
    return (False, s)
runCmd s (Type syn) =
    return (False, s { synonyms = syn : synonyms s })
runCmd s (Set f) =
    return (False, f s)
runCmd s (Query i g) =
    let form = hTypeToFormula (synonyms s) g
	env = [ (Symbol v, hTypeToFormula (synonyms s) t) | (v, t) <- axioms s ]
	mpr = prove env form
    in  case mpr of
	[] -> do
	    putStrLn $ "-- " ++ i ++ " cannot be realized."
	    return (False, s)
	es@(e:_) -> do
	    let es' = if multi s then es else [e]
	    putStrLn $ i ++ " :: " ++ show g
	    mapM_ (putStrLn . hPrClause . termToHClause i) es'
	    return (False, s)

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
	(":delete <sym>",	"Delete from environment.",	pDel),
	(":environment",	"Show environment",		return Env),
	(":help",		"Print this message.",		return (Help False)),
	(":load <file>",	"Load a file",			pLoad),
	(":quit",		"Quit program.",		return Quit),
	(":set",		"Set options",			pSet),
	(":verbose-help",	"Print verbose help.",		return (Help True)),
	("type <sym> <vars> = <type>", "Add a type synonym",	pType),
	("<sym> :: <type>",	"Add to environment",		pAdd),
	("<sym> ? <type>",	"Query",			pQuery),
	("",			"",				return Noop)
	]

getHelp :: (String, String, a) -> String
getHelp (cmd, help, _) = cmd ++ replicate (30 - length cmd) ' ' ++ help

pDel :: ReadP Cmd
pDel = do
    s <- pHSymbol
    return $ Del s

pLoad :: ReadP Cmd
pLoad = do
    s <- pHSymbol
    return $ Load s

pAdd :: ReadP Cmd
pAdd = do
    i <- pHSymbol
    schar ':'
    char ':'
    t <- pHType
    optional $ schar ';'
    return $ Add i t

pQuery :: ReadP Cmd
pQuery = do
    i <- pHSymbol
    schar '?'
    t <- pHType
    optional $ schar ';'
    return $ Query i t

pType :: ReadP Cmd
pType = do
    schar 't'; char 'y'; char 'p'; char 'e'
    (syn:args) <- many1 pHSymbol
    schar '='
    t <- pHType
    return $ Type (syn, (args, t))

pSet :: ReadP Cmd
pSet = (do
    schar '+'
    char 'm'
    return $ Set $ \ s -> s { multi = True }
  ) +++ (do
    schar '-'
    char 'm'
    return $ Set $ \ s -> s { multi = False }
  )

schar :: Char -> ReadP ()
schar c = do
    skipSpaces
    char c
    return ()

helpText :: String
helpText = "\
\Djinn is a program that generates Haskell code from a type.\n\
\Given a type the program will deduce an expression of this type,\n\
\if one exists.  If the Djinn says the type is not realizable it is\n\
\because there is no (total) expression of the given type.\n\
\Djinn only knows about tuples, (), Either, Void, and ->.\n\
\\n\
\Send any comments and feedback to lennart@augustsson.net\n\
\\n\
\Commands (may be abbreviated):\n\
\"
