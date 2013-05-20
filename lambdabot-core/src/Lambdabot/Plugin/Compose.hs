{-# LANGUAGE PatternGuards #-}
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- Another progressive plugin. Compose two (for now) plugins transparently
-- A sort of mini interpreter. Could do with some more thinking.
module Lambdabot.Plugin.Compose (theModule) where

import Lambdabot.Command
import Lambdabot.Monad
import Lambdabot.Plugin

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.List.Split

type Compose = ModuleT () LB

theModule :: Module ()
theModule = newModule
    { moduleCmds = return
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
            , process = \args -> case splitOn " " args of
                (f:g:xs) -> do
                    f' <- lookupP f
                    g' <- lookupP g
                    lb (compose f' g' (concat $ intersperse " " xs)) >>= mapM_ say
                _ -> say "Not enough arguments to @."
            }
        ]
    }

-- | Compose two plugin functions
compose :: (String -> LB [String]) -> (String -> LB [String]) -> (String -> LB [String])
compose f g xs = g xs >>= f . unlines

------------------------------------------------------------------------
-- | Lookup the `process' method we're after, and apply it to the dummy args
--
lookupP :: String -> Cmd Compose (String -> LB [String])
lookupP cmd = withMsg $ \a -> do
    b <- getTarget
    lb $ withCommand cmd
        (error $ "Unknown command: " ++ show cmd)
        (\_m theCmd -> do
            when (privileged theCmd) $ error "Privileged commands cannot be composed"
            bindModule1 (runCommand theCmd a b cmd))


------------------------------------------------------------------------

-- | More interesting composition/evaluation
-- @@ @f x y (@g y z)
evalBracket :: String -> Cmd Compose ()
evalBracket args = do
    cmdPrefixes <- getConfig commandPrefixes

    let conf = cmdPrefixes
    xs <- mapM evalExpr (fst (parseBracket 0 True args conf))
    mapM_ (say . addSpace) (concat' xs)
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

-- TODO: rewrite this using parsec or something
-- | Parse a command invocation that can contain parentheses
--   The Int indicates how many brackets must be closed to end the current argument, or 0
--   The Bool indicates if this is a valid location for a character constant
parseBracket :: Int -> Bool -> String -> [String] -> ([Expr],String)
parseBracket 0 _ [] _       = ([],[])
parseBracket _ _ [] _       = error "Missing ')' in nested command"
parseBracket 1 _ (')':xs) _ = ([],xs)
parseBracket n _ (')':xs) c | n > 0
                            = first (addArg ")") $ parseBracket (n-1) True xs c
parseBracket n _ ('(':xs) c | Just ys <- isCommand xs c     -- (@cmd arg arg)
                            = parseCommand n ys c
parseBracket n _ ('(':xs) c | n > 0
                            = first (addArg "(") $ parseBracket (n+1) True xs c
parseBracket n _ xs c       | Just ('(':ys) <- isCommand xs c -- @(cmd arg arg)
                            = parseCommand n ys c
parseBracket n _ xs c       | Just ys <- isCommand xs c       -- @cmd arg arg
                            = parseInlineCommand n ys c
parseBracket n c (x:xs) cfg | x `elem` "\"'" && (c || x /= '\'')
                            = let (str, ys) = parseString x xs
                                  (rest,zs) = parseBracket n True ys cfg
                              in  (addArg (x:str) rest, zs)
parseBracket n c (x:xs) cfg = first (addArg [x])
                            $ parseBracket n (not (isAlphaNum x) && (c || x /= '\'')) xs cfg

parseCommand, parseInlineCommand :: Int -> String -> [String] -> ([Expr],String)
parseCommand n xs conf = (Cmd cmd args:rest, ws)
    where
        (cmd, ys) = break (`elem` " )") xs
        (args,zs) = parseBracket 1 True (dropWhile (==' ') ys) conf
        (rest,ws) = parseBracket n True zs conf

parseInlineCommand n xs conf = (Cmd cmd rest:[], zs)
  where
    (cmd, ys) = break (`elem` " )") xs
    (rest,zs) = parseBracket n True (dropWhile (==' ') ys) conf

parseString :: Char -> String -> (String, String)
parseString _     []          = ([],[])
parseString delim ('\\':x:xs) = first (\ys -> '\\':x:ys) (parseString delim xs)
parseString delim (x:xs)
  | delim == x                = ([x],xs)
  | otherwise                 = first (x:) (parseString delim xs)


-- | Does xs start with a command prefix?
isCommand :: String -> [String] -> Maybe String
isCommand xs = msum . map dropPrefix
 where dropPrefix p
          | p `isPrefixOf` xs = Just $ drop (length p) xs
          | otherwise         = Nothing

addArg :: String -> [Expr] -> [Expr]
addArg s (Arg a:es) = Arg (s++a):es
addArg s es         = Arg s     :es
