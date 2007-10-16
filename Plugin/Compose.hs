--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- Another progressive plugin. Compose two (for now) plugins transparently
-- A sort of mini interpreter. Could do with some more thinking.
--
module Plugin.Compose (theModule) where

import Plugin
import Message

import Control.Monad.State
import Control.Arrow (first)
import GHC.IOBase   (Exception(NoMethodError))

PLUGIN Compose

instance Module ComposeModule () where
    moduleCmds _   = [".", "compose", "@", "?"]
    moduleHelp _ c = unlines $
                       if c `elem` ["@","?"]
                        then ["@ [args]."
                             ,"@ executes plugin invocations in its arguments, parentheses can be used."
                             ," The commands are right associative."
                             ," For example:    @ @pl @undo code"
                             ," is the same as: @ (@pl (@undo code))"]
                        else [". <cmd1> <cmd2> [args]."
                             ,". [or compose] is the composition of two plugins"
                             ," The following semantics are used: . f g xs == g xs >>= f"]

    process    _ a b c args
        | c `elem` ["@","?"] = lift $ evalBracket (a,b) args

    process    _ a b _ args = lift $ case split " " args of
        (f:g:xs) -> do
            f' <- lookupP (a,b) f
            g' <- lookupP (a,b) g
            compose f' g' (concat $ intersperse " " xs)

        _ -> return ["Not enough arguments to @."]


-- | Compose two plugin functions
compose :: (String -> LB [String]) -> (String -> LB [String]) -> (String -> LB [String])
compose f g xs = g xs >>= f . unlines

------------------------------------------------------------------------
-- | Lookup the `process' method we're after, and apply it to the dummy args
-- Fall back to process_ if there's no process.
--
lookupP :: Message a => (a, Nick) -> String -> LB (String -> LB [String])
lookupP (a,b) cmd = withModule ircCommands cmd
    (error $ "Unknown command: " ++ show cmd) 
    (\m -> do
        privs <- gets ircPrivCommands -- no priv commands can be composed
        when (cmd `elem` privs) $ error "Privledged commands cannot be composed"
        bindModule1 $ \str -> catchError 
                    (process m a b cmd str)
                    (\ex -> case (ex :: IRCError) of 
                                (IRCRaised (NoMethodError _)) -> process_ m cmd str
                                _ -> throwError ex))


------------------------------------------------------------------------

-- | More interesting composition/evaluation
-- @@ @f x y (@g y z)
evalBracket :: Message a => (a, Nick) -> String -> LB [String]
evalBracket a args = liftM concat' $ mapM (evalExpr a) $ fst $ parseBracket True args
 where concat' ([x]:[y]:xs) = concat' ([x++y]:xs)
       concat' xs           = concat xs

evalExpr :: Message a => (a, Nick) -> Expr -> LB [String]
evalExpr _ (Arg s) = return [s]
evalExpr a (Command c args) = do
     args' <- mapM (evalExpr a) args
     let arg = concat $ concat $ map (intersperse " ") args'
     cmd <- lookupP a c
     cmd arg

------------------------------------------------------------------------

data Expr = Command String [Expr]
          | Arg String
    deriving Show

-- | Parse a command invocation that can contain parentheses
parseBracket :: Bool -> String -> ([Expr],String)
parseBracket True  []       = ([],[])
parseBracket False []       = error "Missing ')' in nested command"
parseBracket False (')':xs) = ([],xs)
parseBracket top   ('(':xxs) | Just xs <- isCommand xxs
                            = -- (@cmd arg arg)
                              let (cmd, ys) = break (`elem` " )") xs
                                  (args,zs) = parseBracket False (dropWhile (==' ') ys)
                                  (rest,ws) = parseBracket top zs
                              in  (Command cmd args:rest, ws)
parseBracket top   (xxs)     | Just xs <- isCommand xxs
                            = case break (`elem` " )") xs of
                                  -- @(cmd arg arg)
                                  ('(':cmd,ys) -> let (args,zs) = parseBracket False (dropWhile (==' ') ys)
                                                      (rest,ws) = parseBracket top zs
                                                  in  (Command cmd args:rest, ws)
                                  -- @cmd arg arg
                                  (cmd,    ys) -> let (rest,zs) = parseBracket top (dropWhile (==' ') ys)
                                                  in  (Command cmd rest:[], zs)
parseBracket top   xxs@(x:xs)
    | x `elem` "\"'"        = let (str, ys) = parseString x xs
                                  (rest,zs) = parseBracket top ys
                              in  (addArg (x:str) rest, zs)
    | otherwise             = let (arg, ys) = break (`elem` " )") xxs
                                  (arg',zs) = case arg of
                                                  [] -> (take 1 ys, drop 1 ys)
                                                  _  -> (arg,       ys)
                                  (rest,ws) = parseBracket top zs
                              in  (addArg arg' rest, ws)

-- | Does xs start with a command prefix?
isCommand :: String -> Maybe String
isCommand xs = msum $ map dropPrefix (commandPrefixes config)
 where dropPrefix p
          | p `isPrefixOf` xs = Just $ drop (length p) xs
          | otherwise         = Nothing

addArg :: String -> [Expr] -> [Expr]
addArg s (Arg a:es) = Arg (s++a):es
addArg s es         = Arg s     :es

parseString :: Char -> String -> (String, String)
parseString _     []          = ([],[])
parseString delim ('\\':x:xs) = first (\ys -> '\\':x:ys) (parseString delim xs)
parseString delim (x:xs)
  | delim == x                = ([x],xs)
  | otherwise                 = first (x:) (parseString delim xs)
