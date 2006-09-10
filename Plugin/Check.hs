--
-- Copyright (c) 6 DonStewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Test a property with QuickCheck
--
module Plugin.Check where

import Plugin
import Lib.Parser

PLUGIN Check

instance Module CheckModule () where
    moduleCmds   _     = ["check"]
    moduleHelp _ _     = "check <expr>\nYou have QuickCheck and 3 seconds. Prove something"
    process _ _ to _ s = ios80 to (check s)

binary :: String
binary = "./quickcheck"

isEval :: String -> Bool
isEval = ((evalPrefixes config) `arePrefixesWithSpaceOf`)

dropPrefix :: String -> String
dropPrefix = dropWhile (' ' ==) . drop 2

check :: String -> IO String
check src = do
    -- first, verify the source is actually a Haskell 98 expression, to
    -- avoid code injection bugs.
    case parseExpr (src ++ "\n") of
        ParseFailed _ e -> return $ " " ++ e
        ParseOk     _   -> do
            (out,err,_) <- popen binary [] (Just src)
            let o = munge out
                e = munge err
            return $ case () of {_
                | null o && null e -> "Terminated\n"
                | null o           -> " " ++ e
                | otherwise        -> " " ++ o
            }
            where munge = expandTab . dropWhile (=='\n') . dropNL . clean_ . take 2048

--
-- Clean up runplugs' output
--
clean_ :: String -> String
clean_ s| Just _        <- no_io      `matchRegex`    s = "No IO allowed\n"
        | Just _         <- terminated `matchRegex`    s = "Terminated\n"
        | Just _         <- hput       `matchRegex`    s = "Terminated\n"
        | Just _         <- stack_o_f  `matchRegex`    s = "Stack overflow\n"
        | Just _         <- loop       `matchRegex`    s = "Loop\n"
        | Just _         <- undef      `matchRegex`    s = "Undefined\n"
        | Just _         <- type_sig   `matchRegex`    s = "Add a type signature\n"
        | Just (_,m,_,_) <- ambiguous  `matchRegexAll` s = m
        | Just (_,_,b,_) <- inaninst   `matchRegexAll` s = clean_ b
        | Just (_,_,b,_) <- irc        `matchRegexAll` s = clean_ b
        | Just (_,m,_,_) <- nomatch    `matchRegexAll` s = m
        | Just (_,m,_,_) <- notinscope `matchRegexAll` s = m
        | Just (_,m,_,_) <- hsplugins `matchRegexAll`  s = m
        | Just (a,_,_,_) <- columnnum `matchRegexAll`  s = a
        | Just (a,_,_,_) <- extraargs `matchRegexAll`  s = a
        | Just (_,_,b,_) <- filename' `matchRegexAll`  s = clean_ b
        | Just (a,_,b,_) <- filename  `matchRegexAll`  s = a ++ clean_ b
        | Just (a,_,b,_) <- filepath `matchRegexAll`   s = a ++ clean_ b
        | Just (a,_,b,_) <- runplugs  `matchRegexAll`  s = a ++ clean_ b
        | otherwise      = s
    where
        -- s/<[^>]*>:[^:]: //
        type_sig   = mkRegex "add a type signature that fixes these type"
        no_io      = mkRegex "No instance for \\(Show \\(IO"
        terminated = mkRegex "waitForProc"
        stack_o_f  = mkRegex "Stack space overflow"
        loop       = mkRegex "runplugs: <<loop>>"
        irc        = mkRegex "\n*<irc>:[^:]*:[^:]*:\n*"
        filename   = mkRegex "\n*<[^>]*>:[^:]*:\\?[^:]*:\\?\n* *"
        filename'  = mkRegex "/tmp/.*\\.hs[^\n]*\n"
        filepath   = mkRegex "\n*/[^\\.]*.hs:[^:]*:\n* *"
        undef      = mkRegex "Prelude.undefined"
        ambiguous  = mkRegex "Ambiguous type variable `a\' in the constraints"
        runplugs   = mkRegex "runplugs: "
        notinscope = mkRegex "Variable not in scope:[^\n]*"
        hsplugins  = mkRegex "Compiled, but didn't create object"
        extraargs  = mkRegex "[ \t\n]*In the [^ ]* argument"
        columnnum  = mkRegex " at <[^\\.]*\\.[^\\.]*>:[^ ]*"
        nomatch    = mkRegex "Couldn't match[^\n]*\n"
        inaninst   = mkRegex "^[ \t]*In a.*$"
        hput       = mkRegex "<stdout>: hPutStr"
