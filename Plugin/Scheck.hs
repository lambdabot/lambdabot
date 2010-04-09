{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, PatternGuards #-}
-- Copyright (c) 6 DonStewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Test a property with SmallCheck
module Plugin.Scheck where

import File (findFile)
import Plugin
import Lambdabot.Parser
import qualified Text.Regex as R

$(plugin "Small")

instance Module SmallModule () where
    moduleCmds   _     = ["scheck"]
    moduleHelp _ _     = "scheck <expr>\nYou have SmallCheck and 3 seconds. Test something."
    process _ _ to _ s = ios80 to (check s)

binary :: String
binary = "mueval"


check :: String -> IO String
check src = do
    case parseExpr src of
        Left e  -> return e
        Right _ -> do
            file <- findFile "L.hs"
            (out,err,_) <- popen binary ["--loadfile=", file, "-XExtendedDefaultRules", "-e", "mysmallcheck " ++ src ++ ""] Nothing
            let o = munge out
                e = munge err
            return $ case () of {_
                                 | null o && null e -> "Terminated\n"
                                 | null o           -> " " ++ e
                                 | otherwise        -> " " ++ o
            }
            where munge = id
                  f []   = []
                  f [x]  = [x]
                  f (x:y)= [x ++ " " ++ (concat . intersperse ", ") y]

clean_ :: String -> String
clean_ s
    |  no_io      `matches'`    s = "No IO allowed\n"
    |  terminated `matches'`    s = "Terminated\n"
    |  hput       `matches'`    s = "Terminated\n"
    |  stack_o_f  `matches'`    s = "Stack overflow\n"
    |  loop       `matches'`    s = "Loop\n"
    |  undef      `matches'`    s = "Undefined\n"
    |  type_sig   `matches'`    s = "Add a type signature\n"

    | Just (_,m,_,_) <- ambiguous  `R.matchRegexAll` s = m
    | Just (_,_,b,_) <- inaninst   `R.matchRegexAll` s = clean_ b
    | Just (_,_,b,_) <- irc        `R.matchRegexAll` s = clean_ b
    | Just (_,m,_,_) <- nomatch    `R.matchRegexAll` s = m
    | Just (_,m,_,_) <- notinscope `R.matchRegexAll` s = m
    | Just (_,m,_,_) <- hsplugins  `R.matchRegexAll`  s = m
    | Just (a,_,_,_) <- columnnum  `R.matchRegexAll`  s = a
    | Just (a,_,_,_) <- extraargs  `R.matchRegexAll`  s = a
    | Just (_,_,b,_) <- filename'  `R.matchRegexAll`  s = clean_ b
    | Just (a,_,b,_) <- filename   `R.matchRegexAll`  s = a ++ clean_ b
    | Just (a,_,b,_) <- filepath   `R.matchRegexAll`   s = a ++ clean_ b
    | Just (a,_,b,_) <- runplugs   `R.matchRegexAll`  s = a ++ clean_ b

    | otherwise      = s

    where
        -- s/<[^>]*>:[^:]: //
        type_sig   = regex' "add a type signature that fixes these type"
        no_io      = regex' "No instance for \\(Show \\(IO"
        terminated = regex' "waitForProc"
        stack_o_f  = regex' "Stack space overflow"
        loop       = regex' "runplugs: <<loop>>"
        irc        = regex' "\n*<irc>:[^:]*:[^:]*:\n*"
        filename   = regex' "\n*<[^>]*>:[^:]*:\\?[^:]*:\\?\n* *"
        filename'  = regex' "/tmp/.*\\.hs[^\n]*\n"
        filepath   = regex' "\n*/[^\\.]*.hs:[^:]*:\n* *"
        undef      = regex' "Prelude.undefined"
        ambiguous  = regex' "Ambiguous type variable `a\' in the constraints"
        runplugs   = regex' "runplugs: "
        notinscope = regex' "Variable not in scope:[^\n]*"
        hsplugins  = regex' "Compiled, but didn't create object"
        extraargs  = regex' "[ \t\n]*In the [^ ]* argument"
        columnnum  = regex' " at <[^\\.]*\\.[^\\.]*>:[^ ]*"
        nomatch    = regex' "Couldn't match[^\n]*\n"
        inaninst   = regex' "^[ \t]*In a.*$"
        hput       = regex' "<stdout>: hPutStr"
