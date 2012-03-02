{-# LANGUAGE TemplateHaskell, PatternGuards, ViewPatterns #-}
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- Another progressive plugin. Compose two (for now) plugins transparently
-- A sort of mini interpreter. Could do with some more thinking.
module Plugin.Compose (theModule) where

import Plugin
import Lambdabot.Command

import Control.Monad.State
import Control.Arrow (first)

plugin "Compose"

instance Module ComposeModule where
    moduleCmds _   = 
        [ (command "@")
            { aliases = ["?"]
            , help = do
                c <- getCmdName
                let cc = c++c
                mapM_ say
                    [ cc++" [args]."
                    , cc++" executes plugin invocations in its arguments, parentheses can be used."
                    , " The commands are right associative."
                    , " For example:    "++cc++" "++c++"pl "++c++"undo code"
                    , " is the same as: "++cc++" ("++c++"pl ("++c++"undo code))"
                    ]
            , process = evalBracket
            }
        , (command ".")
            { aliases = ["compose"]
            , help = mapM_ say
                [ ". <cmd1> <cmd2> [args]."
                , ". [or compose] is the composition of two plugins"
                , " The following semantics are used: . f g xs == g xs >>= f"
                ]
            , process = \args -> case split " " args of
                (f:g:xs) -> do
                    f' <- lookupP f
                    g' <- lookupP g
                    lift (lift $ compose f' g' (concat $ intersperse " " xs)) >>= mapM_ say
                _ -> say "Not enough arguments to @."
            }
        ]


-- | Compose two plugin functions
compose :: (String -> LB [String]) -> (String -> LB [String]) -> (String -> LB [String])
compose f g xs = g xs >>= f . unlines

------------------------------------------------------------------------
-- | Lookup the `process' method we're after, and apply it to the dummy args
--
lookupP :: String -> Cmd Compose (String -> LB [String])
lookupP cmd = withMsg $ \a -> do
    b <- getTarget
    lift $ lift $ withModule ircCommands cmd
        (error $ "Unknown command: " ++ show cmd)
        (\m -> do
            privs <- gets ircPrivCommands -- no priv commands can be composed
            let Just theCmd = lookupCmd m cmd
            when (privileged theCmd) $ error "Privileged commands cannot be composed"
            bindModule1 (runCommand theCmd a b cmd))


------------------------------------------------------------------------

-- | More interesting composition/evaluation
-- @@ @f x y (@g y z)
evalBracket :: String -> Cmd Compose ()
evalBracket args = 
    mapM_ say =<< (liftM (map addSpace . concat') $ mapM evalExpr $ fst $ parseBracket 0 True args)
 where concat' ([x]:[y]:xs) = concat' ([x++y]:xs)
       concat' xs           = concat xs

       addSpace :: String -> String
       addSpace (' ':xs) = ' ':xs
       addSpace xs       = ' ':xs

evalExpr :: Expr -> Cmd Compose [String]
evalExpr (Arg s) = return [s]
evalExpr (Cmd c args) = do
     args' <- mapM evalExpr args
     let arg = concat $ concat $ map (intersperse " ") args'
     cmd <- lookupP c
     lift (lift (cmd arg))

------------------------------------------------------------------------

data Expr = Cmd String [Expr]
          | Arg String
    deriving Show

-- | Parse a command invocation that can contain parentheses
--   The Int indicates how many brackets must be closed to end the current argument, or 0
--   The Bool indicates if this is a valid location for a character constant
parseBracket :: Int -> Bool -> String -> ([Expr],String)
parseBracket 0 _ []       = ([],[])
parseBracket _ _ []       = error "Missing ')' in nested command"
parseBracket 1 _ (')':xs) = ([],xs)
parseBracket n _ (')':xs) | n > 0
                          = first (addArg ")") $ parseBracket (n-1) True xs
parseBracket n _ ('(':xs) | Just ys <- isCommand xs       -- (@cmd arg arg)
                          = parseCommand n ys
parseBracket n _ ('(':xs) | n > 0
                          = first (addArg "(") $ parseBracket (n+1) True xs
parseBracket n _ xs       | Just ('(':ys) <- isCommand xs -- @(cmd arg arg)
                          = parseCommand n ys
parseBracket n _ xs       | Just ys <- isCommand xs       -- @cmd arg arg
                          = parseInlineCommand n ys
parseBracket n c (x:xs)   | x `elem` "\"'" && (c || x /= '\'')
                          = let (str, ys) = parseString x xs
                                (rest,zs) = parseBracket n True ys
                            in  (addArg (x:str) rest, zs)
parseBracket n c (x:xs)   = first (addArg [x])
                          $ parseBracket n (not (isAlphaNum x) && (c || x /= '\'')) xs

parseCommand, parseInlineCommand :: Int -> String -> ([Expr],String)
parseCommand n xs = let (cmd, ys) = break (`elem` " )") xs
                        (args,zs) = parseBracket 1 True (dropWhile (==' ') ys)
                        (rest,ws) = parseBracket n True zs
                    in  (Cmd cmd args:rest, ws)

parseInlineCommand n xs = let (cmd, ys) = break (`elem` " )") xs
                              (rest,zs) = parseBracket n True (dropWhile (==' ') ys)
                          in  (Cmd cmd rest:[], zs)

parseString :: Char -> String -> (String, String)
parseString _     []          = ([],[])
parseString delim ('\\':x:xs) = first (\ys -> '\\':x:ys) (parseString delim xs)
parseString delim (x:xs)
  | delim == x                = ([x],xs)
  | otherwise                 = first (x:) (parseString delim xs)


-- | Does xs start with a command prefix?
isCommand :: String -> Maybe String
isCommand xs = msum $ map dropPrefix (commandPrefixes config)
 where dropPrefix p
          | p `isPrefixOf` xs = Just $ drop (length p) xs
          | otherwise         = Nothing

addArg :: String -> [Expr] -> [Expr]
addArg s (Arg a:es) = Arg (s++a):es
addArg s es         = Arg s     :es
