module DummyModule (DummyModule, dummyModule, theModule) where

import IRC

newtype DummyModule = DummyModule ()

theModule :: MODULE
theModule = MODULE dummyModule

dummyModule :: DummyModule
dummyModule = DummyModule ()

instance Module DummyModule where
  moduleName   _ = return "dummy"
  moduleSticky _ = False

  moduleHelp _ s = return $ case s of
        "dummy"       -> "dummy module"
        "wiki"        -> "wiki urls"
        "learn"       -> "another url"
        "eurohaskell" -> "urls are good"
        "moo"         -> "vegan-friendly command"
        _             -> "dummy module"

  commands     _ = return ["dummy","wiki","learn","eurohaskell","moo"]
  process _ _ src "dummy" _    = ircPrivmsg src "dummy"
  process _ _ src "eurohaskell" _ = ircPrivmsg src "less talks, more code!\nhttp://www.haskell.org/hawiki/EuroHaskell\nEuroHaskell - Haskell Hackfest - Summer 2005 - Gothenburg, Sweden"
  process _ _ src "wiki" rest  = ircPrivmsg src ("http://www.haskell.org/hawiki/" ++ rest)
  process _ _ src "learn" _    = ircPrivmsg src "http://www.haskell.org/learning.html"
  process _ _ src "moo" _      = ircPrivmsg src $ unlines ["         (__)","         (oo)","   /------\\/","  / |    ||"," *  /\\---/\\","    ~~   ~~","....\"Have you mooed today?\"..." ]

  process _ _ _ _ _ = error  "Dummy: invalid pattern"
