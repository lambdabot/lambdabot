{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, PatternGuards #-}
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
module Plugin.Type where
import File
import Plugin
import qualified Text.Regex as R

$(plugin "Type")

instance Module TypeModule () where
     moduleCmds        _  = ["type", "kind"]
     moduleHelp _ "kind"  = "kind <type>. Return the kind of a type"
     moduleHelp _ _       = "type <expr>. Return the type of a value"
     process_ _ s expr = lift $ flip query_ghci expr $ case s of
                                                    "type" -> ":t"
                                                    "kind" -> ":k"

     contextual  _ _ _ text = case () of
        _| ":t " `isPrefixOf` text -> lift $ query_ghci ":t" expr
         | ":k " `isPrefixOf` text -> lift $ query_ghci ":k" expr
         | otherwise               -> return []
         where expr = drop 3 text

--     In accordance with the KISS principle, the plan is to delegate all
--     the hard work! To get the type of foo, pipe

command :: [Char] -> [Char] -> [Char]
command cmd foo = cmd ++ " " ++ foo

--     into hugs and send any line matching

signature_regex :: Regex
signature_regex
    = R.mkRegexWithOpts
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
--     We filtering out the lines that match our regex,
--     selecting the last subset match on each matching line before finally concatting
--     the whole lot together again.
--
extract_signatures :: String -> Maybe String
extract_signatures output
        = fmap reverse . removeExp . reverse .
          unwords . map (dropWhile isSpace . expandTab) .
          mapMaybe ((>>= last') . R.matchRegex signature_regex) .
          lines $ output
        where
        last' [] = Nothing
        last' xs = Just $ last xs

        removeExp :: String -> Maybe String
        removeExp [] = Nothing
        removeExp xs = removeExp' 0 xs

        removeExp' :: Int -> String -> Maybe String
        removeExp' 0 (' ':':':':':' ':_) = Just []
        removeExp' n ('(':xs)            = ('(':) `fmap` removeExp' (n+1) xs
        removeExp' n (')':xs)            = (')':) `fmap` removeExp' (n-1) xs
        removeExp' n (x  :xs)            = (x  :) `fmap` removeExp'  n    xs
        removeExp' _ []                  = Nothing

--
--     With this the command handler can be easily defined using popen:
--
-- TODO, bring more modules into scope.
--
query_ghci' :: String -> String -> IO String
query_ghci' cmd expr = do
       importHeader <- findFile "imports.h"
       imports <- fmap (map (unwords . drop 1 . words)
                        . filter (null
                                  . intersect ["as","hiding","qualified"]
                                  . words)
                        . filter (isPrefixOf "import")
                        . lines)
                       (readFile importHeader)
       l <- findFile "L.hs"
       let context = ":load "++l++"\n" ++ concatMap ((":m + " ++) . (++"\n")) imports
       (output, errors, _) <- popen (ghci config) ["-v0","-fglasgow-exts","-XNoTemplateHaskell","-iState","-iscripts","-XNoMonomorphismRestriction"]
                                       (Just (context ++ command cmd (stripComments expr)))
       let ls = extract_signatures output
       return $ case ls of
                  Nothing -> unlines . take 3 . filter (not . null) . map cleanRE2 .
                             lines . expandTab . cleanRE . filter (/='\r') $ errors -- "bzzt"
                  Just t -> t
  where
     {-
     context = ":l L\n" ++ (concatMap (\m -> ":m + "++m++"\n") $
                    prehier ++ datas ++ qualifieds ++ controls ++ other ++ extras)

     other      =
        ["Text.Printf"
        ,"Text.PrettyPrint.HughesPJ"]

     prehier    = ["Numeric"]

     qualifieds = []

     datas   = map ("Data." ++)
        ["Array"
        ,"Bits"
        ,"Bool"
        ,"Char"
        ,"Complex"
        ,"Dynamic"
        ,"Either"
        ,"Eq"
        ,"Fixed"
    --  ,"Foldable"
    --  ,"Function"
    --  ,"Generics"
        ,"Graph"
        ,"Int"
    --  ,"IntMap"
    --  ,"IntSet"
        ,"Ix"
        ,"List"
    --  ,"Map"
        ,"Maybe"
        ,"Monoid"
        ,"Ord"
        ,"Ratio"
    --  ,"Set"
        ,"Tree"
        ,"Tuple"
        ,"Typeable"
        ,"Word"
        ]
     extras   = [] -- ["L"]

     controls = map ("Control." ++)
        ["Monad"
        ,"Monad.Cont"
        ,"Monad.Error"
        ,"Monad.Identity"
        ,"Monad.List"
        ,"Monad.RWS"
        ,"Monad.Reader"
        ,"Monad.State"
        ,"Monad.Trans"
        ,"Monad.Writer"
        ,"Monad.Fix"
        ,"Monad.Instances"
        ,"Applicative"
        ,"Arrow"
    --  ,"Arrow.Transformer"
    --  ,"Arrow.Transformer.All"
    --  ,"Arrow.Operations"
        ,"Parallel"
        ,"Parallel.Strategies"
        ]
     -}

     cleanRE, cleanRE2 :: String -> String
     cleanRE s
        | Just _         <- notfound `R.matchRegex` s = "Couldn\'t find qualified module."
        | Just (_,_,b,_) <- ghci_msg `R.matchRegexAll`  s = b
        | otherwise      = s
     cleanRE2 s
        | Just (_,_,b,_) <- ghci_msg `R.matchRegexAll`  s = b
        | otherwise      = s
     ghci_msg = R.mkRegex "<interactive>:[^:]*:[^:]*: ?"
     notfound = R.mkRegex "Failed to load interface"

query_ghci :: String -> String -> LB [String]
query_ghci y z = ios (query_ghci' y z)
