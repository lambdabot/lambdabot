--
-- | Simple template module
-- Contains many constant bot commands.
--
module Plugin.Dummy (theModule) where

import Plugin

import Plugin.Dummy.DocAssocs (docAssocs)
import Plugin.Dummy.Moo (cows)

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P

PLUGIN Dummy

instance Module DummyModule [String] where
  moduleDefState = const . return . cycle $ cows

  moduleCmds   _ = "eval" : {-"moo" : -} map fst dummylst

  moduleHelp _ s = case s of
        "dummy"       -> "dummy. Print a string constant"
        "eval"        -> "eval. Do nothing (perversely)"

        "id"          -> "id <arg>. The identiy plugin"
        "wiki"        -> "wiki <page>. URLs of Haskell wiki pages"
        "oldwiki"     -> "oldwiki <page>. URLs of the old hawiki pages"
        "paste"       -> "paste. Paste page url"

        "docs"        -> "docs <lib>. Lookup the url for this library's documentation"
        "libsrc"      -> "libsrc <lib>. Lookup the url of fptools libraries"
        "fptools"     -> "fptools <lib>. Lookup url of ghc base library modules"

        "learn"       -> "learn. The learning page url."
        "eurohaskell" -> "eurohaskell. Historical."
        "moo"         -> "moo. Vegans rock!"
        "map"         -> "map. #haskell user map"
        "botsnack"    -> "botsnakc. Feeds the bot a snack."
        "get-shapr"   -> "get-shapr. Summon shapr instantly"
        "shootout"    -> "shootout. The debian language shootout"
        "faq"         -> "faq. Answer frequently asked questions about Haskell"


{-
  process _ _ src "moo" _ = do
        cow' <- withMS $ \(cow:farm) writer -> do
          writer farm
          return cow
        mapM_ (ircPrivmsg' src) (lines cow')
-}
  process_ _ "eval" _ = return []

dummylst :: [(String, String -> String)]
dummylst = 
    [("id",         id)

    ,("dummy",      const "dummy")
    ,("get-shapr",  const "shapr!!")
    ,("faq",        const "The answer is: Yes! Haskell can do that.")
    ,("paste",      const "http://www.haskell.org/hawiki/HaskellIrcPastePage")
    ,("learn",      const "http://www.haskell.org/learning.html")
    ,("map",        const "http://www.haskell.org/hawiki/HaskellUserLocations")
    ,("shootout",   const "http://shootout.alioth.debian.org/gp4/benchmark.php?test=all&lang=all")
    ,("botsnack",   const ":)")

    ,("eurohaskell", const "less talks, more code!\n\ 
                          \http://www.haskell.org/hawiki/EuroHaskell\n\ 
                          \EuroHaskell - Haskell Hackfest - Summer 2005 - Gothenburg, Sweden")

    ,("wiki",        ("http://www.haskell.org/haskellwiki/" ++))
    ,("oldwiki",     ("http://www.haskell.org/hawiki/" ++))

    ,("docs",        \x -> case x of
       [] -> "http://haskell.org/ghc/docs/latest/html/libraries/index.html"
       _  -> case M.lookup (P.pack x) docAssocs of
             Nothing -> x ++ " not available"
             Just m  -> "http://haskell.org/ghc/docs/latest/html/libraries/" <>
                        (P.unpack m) </> map (choice (=='.') (const '-') id) x <.> "html")

    -- broken:
    ,("libsrc",      \x -> case M.lookup (P.pack x) docAssocs of
       Nothing -> x ++ " not available"
       Just m  -> "http://darcs.complete.org/fptools/libraries/" <>
                  (P.unpack m) </> map (choice (=='.') (const '/') id) x <.> "hs")

    ,("fptools",     \x -> case M.lookup (P.pack x) docAssocs of
       Nothing -> x ++ " not available"
       Just m  -> "http://darcs.haskell.org/packages/" <>
                  (P.unpack m) </> map (choice (=='.') (const '/') id) x <.> "hs")
    ]
