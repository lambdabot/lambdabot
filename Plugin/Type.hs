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
module Plugin.Type where

import Plugin

PLUGIN Type

instance Module TypeModule () where
     moduleCmds        _  = ["type", "kind"]
     moduleHelp _ "kind"  = "kind <type>. Return the kind of a type"
     moduleHelp _ _       = "type <expr>. Return the type of a value"
     process_ _ s expr = lift $ flip query_ghci expr $ case s of
                                                    "type" -> ":t"
                                                    "kind" -> ":k"

--     In accordance with the KISS principle, the plan is to delegate all
--     the hard work! To get the type of foo, pipe

command :: [Char] -> [Char] -> [Char]
command cmd foo = cmd ++ " " ++ foo

--     into hugs and send any line matching

signature_regex :: Regex
signature_regex
    = mkRegexWithOpts
      "^(\\*?[A-Z][_a-zA-Z0-9]*(\\*?[A-Z][_a-zA-Z0-9]*)*>)? *(.*[       -=:].*)"
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
--     We first strip 7 leading lines, which is the GHCi logo, then the final line,
--     which is the last prompt, before filtering out the lines that match our regex,
--     selecting the last subset match on each matching line before finally concatting
--     the whole lot together again.
--
-- TODO, just use ghci -v0
--
extract_signatures :: String -> String
extract_signatures output
        = removeExp . concat . intersperse " " . map (dropWhile isSpace . expandTab) .
          mapMaybe ((>>= last') . matchRegex signature_regex) .
          reverse . drop 1 . reverse . drop 7 . lines $ output
        where
        last' [] = Nothing
        last' xs = Just $ last xs

        removeExp :: String -> String
        removeExp (' ':':':':':' ':xs) = xs
        removeExp xs = case lex xs of
          [("(",ys)] -> removeExp $ stripParens 1 ys
          [("","")]  -> []
          [(_,ys)]   -> removeExp ys
          _          -> error "invalid ghci output: unexpected lex behavior"

        stripParens :: Int -> String -> String
        stripParens 0 xs = xs
        stripParens n xs = case lex xs of
          [("(",ys)] -> stripParens (n+1) ys
          [(")",ys)] -> stripParens (n-1) ys
          [("","")]  -> error "invalid ghci output: open parenthesis"
          [(_,ys)]   -> stripParens n     ys
          _          -> error "invalid ghci output: unexpected lex behavior"

--
--     With this the command handler can be easily defined using popen:
--
-- TODO, bring more modules into scope.
--
query_ghci' :: String -> String -> IO String
query_ghci' cmd expr = do
       (output, errors, _) <- popen (ghci config) ["-fglasgow-exts","-fno-th"]
                                       (Just (context ++ command cmd (stripComments expr)))
       let ls = extract_signatures output
       return $ if null ls
                then unlines . take 3 . lines . expandTab . cleanRE $ errors -- "bzzt" 
                else ls
  where
     context = concatMap (\m -> ":m + "++m++"\n") $
                    prehier ++ datas ++ qualifieds ++ controls ++ other

     other      = ["Text.Printf"]
     prehier    = ["Char", "List", "Maybe", "Numeric", "Random" ]
     qualifieds = []
     datas   = map ("Data." ++) [
                    "Array",
                    "Bits", "Bool", "Char", "Dynamic", "Either",
                    "Graph", "Int", "Ix", "List",
                    "Maybe", "Ratio", "Tree", "Tuple", "Typeable", "Word"
                  ]
     controls = map ("Control." ++) ["Monad", "Monad.State", "Monad.Reader", "Monad.Fix", "Arrow"]

     cleanRE :: String -> String
     cleanRE s
        | Just _         <- notfound `matchRegex` s = "Couldn\'t find qualified module."
        | Just (_,_,b,_) <- ghci_msg `matchRegexAll`  s = b
        | otherwise      = s
     ghci_msg = mkRegex "<interactive>:[^:]*:[^:]*: ?"
     notfound = mkRegex "Failed to load interface"

query_ghci :: String -> String -> LB [String]
query_ghci y z = ios (query_ghci' y z)
