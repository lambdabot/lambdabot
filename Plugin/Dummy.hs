{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | Simple template module
-- Contains many constant bot commands.
module Plugin.Dummy (theModule) where

import Plugin

import Plugin.Dummy.DocAssocs (docAssocs)

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P

$(plugin "Dummy")

instance Module DummyModule () where

  moduleCmds   _ = "eval" : "choose" : map fst dummylst

  moduleHelp _ s = case s of
        "dummy"       -> "dummy. Print a string constant"
        "bug"         -> "bug. Submit a bug to GHC's trac"
        "eval"        -> "eval. Do nothing (perversely)"

        "id"          -> "id <arg>. The identity plugin"
        "read"        -> "read \"<foo>\". Print <foo>"
        "show"        -> "show <foo>. Print \"<foo>\""
        "wiki"        -> "wiki <page>. URLs of Haskell wiki pages"
        "oldwiki"     -> "oldwiki <page>. URLs of the old hawiki pages"
        "paste"       -> "paste. Paste page url"

        "docs"        -> "docs <lib>. Lookup the url for this library's documentation"
        "source"      -> "source <lib>. Lookup the url of fptools libraries"
        "fptools"     -> "fptools <lib>. Lookup url of ghc base library modules"

        "learn"       -> "learn. The learning page url"
        "eurohaskell" -> "eurohaskell. Historical"
        "map"         -> "map. #haskell user map"
        "botsnack"    -> "botsnack. Feeds the bot a snack"
        "get-shapr"   -> "get-shapr. Summon shapr instantly"
        "shootout"    -> "shootout. The debian language shootout"
        "faq"         -> "faq. Answer frequently asked questions about Haskell"
        "choose"      -> "choose. Lambdabot featuring AI power"
        "googleit"    -> "letmegooglethatforyou."

  process_ _ "eval"   _    = return []
  process_ _ "choose" []   = return ["Choose between what?"]
  process_ _ "choose" xs   = fmap return . io . randomElem . lines $ xs
  process_ _ cmd      rest = case lookup cmd dummylst of
    Nothing -> error "Dummy: invalid command"
    Just f  -> return $ lines $ f rest

  contextual _ _ _ "lisppaste2: url" = return [pastebinMsg]
  contextual _ _ _ _                 = return []

pastebinMsg :: String
pastebinMsg = "Haskell pastebin: http://hpaste.org/"

dummylst :: [(String, String -> String)]
dummylst =
    [("id",      (' ' :) . id)
    ,("read",    (' ' :) . filter (/= '\n') . read)
    ,("show",       show)

    ,("dummy",      const "dummy")
    ,("bug",        const "http://hackage.haskell.org/trac/ghc/newticket?type=bug")
    ,("get-shapr",  const "shapr!!")
    ,("faq",        const "The answer is: Yes! Haskell can do that.")
    ,("paste",      const pastebinMsg)
    ,("learn",      const "http://www.haskell.org/learning.html")
    ,("map",        const "http://www.haskell.org/hawiki/HaskellUserLocations")
    ,("shootout",   const "http://shootout.alioth.debian.org/gp4/benchmark.php?test=all&lang=all")
    ,("botsnack",   const ":)")
    ,("thanks",     const "you are welcome")
    ,("thx",        const "you are welcome")
    ,("thank you",  const "you are welcome")
    ,("ping",	    const "pong")

    ,("wiki",       lookupWiki)
    ,("oldwiki",     ("http://www.haskell.org/hawiki/" ++))

    ,("docs",        \x -> if null x
                            then docPrefix <> "index.html"
                            else lookupPackage docPrefix '-' "html" x)

    ,("source",     lookupPackage "http://darcs.haskell.org/packages/" '/' "hs")

    ,("fptools",    lookupPackage "http://darcs.haskell.org/packages/" '/' "hs")
    ,("hackage",    lookupHackage)
    ,("googleit",   lookupGoogle)
    ]

lookupWiki :: String -> String
lookupWiki page = "http://www.haskell.org/haskellwiki/" ++ spacesToUnderscores page
  where spacesToUnderscores = map (\c -> if c == ' ' then '_' else c)

lookupHackage :: String -> String
lookupHackage "" = "http://hackage.haskell.org"
lookupHackage xs = "http://hackage.haskell.org/package/" ++ xs

googlePrefix :: String
googlePrefix = "http://letmegooglethatforyou.com"

lookupGoogle :: String -> String
lookupGoogle "" = googlePrefix
lookupGoogle xs = googlePrefix ++ "/?q=" ++ quote xs
 where
    quote = map (\x -> if x == ' ' then '+' else x)

docPrefix :: String
docPrefix = "http://haskell.org/ghc/docs/latest/html/libraries/"

lookupPackage :: String -> Char -> String -> String -> String
lookupPackage begin sep end x'
 = case M.lookup (P.pack x) docAssocs of
        Nothing -> x ++ " not available"
        Just m  -> begin
                    <> P.unpack m
                    </> map (choice (=='.') (const sep) id) x
                    <.> end
 where x = dropSpace x'
