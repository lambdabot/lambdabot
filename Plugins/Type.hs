--
-- |   The Type Module - another progressive plugin for lambdabot
--
-- pesco hamburg 2003-04-05
--
--     Greetings reader,
--
--     whether you're a regular follower of the series or dropping in for
--     the first time, let me present for your pleasure the Type Module:
--
--     One thing we enjoy on #haskell is throwing function types at each
--     other instead of spelling out tiresome monologue about arguments
--     or return values. Unfortunately such a toss often involves a local
--     lookup of the type signature in question because one is seldom
--     sure about the actual argument order.
--
--     Well, what do you know, this plugin enables lambdabot to automate
--     that lookup for you and your fellow lambda hackers.
--
module Plugins.Type (theModule) where

import Lambdabot
import Util                 (expandTab)
import PosixCompat          (popen)

import Maybe (mapMaybe)
import Control.Monad.Trans (liftIO)
import Text.Regex          (Regex, mkRegexWithOpts, matchRegex)

--     In accordance with the KISS principle, the plan is to delegate all
--     the hard work! To get the type of foo, pipe

command :: [Char] -> [Char] -> [Char]
command cmd foo = cmd ++ " " ++ foo

--     into hugs and send any line matching

signature_regex :: Regex
signature_regex
    = mkRegexWithOpts
      "^(\\*?[A-Z][_a-zA-Z0-9]*(\\*?[A-Z][_a-zA-Z0-9]*)*>)? *(.*[	-=:].*)"
      True True

--
-- Rather than use subRegex, which is new to 6.4, we can remove comments
-- old skool style.
-- Former regex for this:
--    "(\\{-[^-]*-+([^\\}-][^-]*-+)*\\}|--.*$)"
--
stripComments :: String -> String
stripComments []          = []
stripComments ('\n':_)    = [] -- drop any newwline and rest. *security*
stripComments ('-':'-':_) = []  -- 
stripComments ('{':'-':cs)= stripComments (go 1 cs)
stripComments (c:cs)      = c : stripComments cs

-- Adapted from ghc/compiler/parser/Lexer.x
go :: Int -> String -> String
go 0 xs         = xs
go _ ('-':[])   = []   -- unterminated
go n ('-':x:xs) 
    | x == '}'  = go (n-1) xs
    | otherwise = go n (x:xs)
go _ ('{':[])   = []  -- unterminated
go n ('{':x:xs)
    | x == '-'  = go (n+1) xs
    | otherwise = go n (x:xs)
go n (_:xs) = go n xs
go _ _      = []   -- unterminated

--     through IRC.

--
--     To get any signature line from the hugs output, split it into lines,
--     match each against the regex, and take the last substring match from
--     each successful match.
--
extract_signatures :: String -> [String]
extract_signatures output
        = map expandTab . mapMaybe last' . mapMaybe (matchRegex signature_regex) .
                 reverse . drop 1 . reverse . drop 7 . lines $ output
        where
        last' [] = Nothing
        last' xs = Just $ last xs

--
--     With this the command handler can be easily defined using popen:
--
query_ghci :: String -> String -> String -> LB ()
query_ghci src cmd expr =
       do
       (output, _, _) <- liftIO $ popen "ghci-6.4" ["-fglasgow-exts","-fno-th"]
			                  (Just (command cmd (stripComments expr)))
       let ls = extract_signatures output
       ircPrivmsg src . unlines $ if null ls then ["bzzt"] else ls

--
--     And thus the plugin:
--
newtype TypeModule = TypeModule ()

theModule :: MODULE
theModule = MODULE $ TypeModule ()

instance Module TypeModule () where
     moduleHelp _ "type" = return "@type: return the type of a value"
     moduleHelp _ "kind" = return "@kind: return the kind of a type (GHC)"
     moduleHelp _ _      = return "@type,@kind: interact with the typechecker"
     moduleCmds        _ = return ["type", "kind"]
     process _ _ src "type" expr = query_ghci src ":t" expr
     process _ _ src "kind" expr = query_ghci src ":k" expr
--   process _ _ src "info" expr = query_ghci src ":info" expr
     process _ _ _ _ _ = error "TypeModule: invalid cmd"

