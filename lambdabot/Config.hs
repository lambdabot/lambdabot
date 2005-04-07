
module Config where

import qualified Map
import Data.Maybe               (fromMaybe)

data Config = Config {
        name      :: String,
        userinfo  :: String,
        host      :: String,
        port      :: Int,
        verbose   :: Bool,
        moresize  :: Int,
        autojoin  :: [String],
        admins    :: [String]
}
        
--
-- Useful defaults for #haskell.
--
config :: Config
config = Config { 
        name            = "lambdabot",
        userinfo        = "Lambda Robots - 100% Loyal",
        host            = "irc.eu.freenode.net",
        port            = 6667,
        verbose         = True,
        moresize        = 7,
        autojoin        = ["#haskell"],

        admins          = [
                "Pseudonym",    "shapr",        "pesco",        "Riastradh",
                "Darius",       "tmoertel",     "delYsid",      "polli", 
                "Heffalump",    "Igloo",        "Marvin--",     "o3",     
                "phubuh",       "ddarius",      "bringert",     "dons",     
                "TheHunter",    "jlouis"
        ]
   }

------------------------------------------------------------------------
--
-- the rest of this should be generated into Modules.hs by genModules...
-- including the dependencies.
--

getModuleFile :: String -> String
getModuleFile s = 
        fromMaybe (error "unknown module") (Map.lookup s modules)

modules :: Map.Map String String
modules = Map.fromList
        [("fact"   , "FactModule.o")
        ,("hello"  , "HelloModule.o")
        ,("state"  , "StateModule.o")
        ,("topic"  , "TopicModule.o")
        ,("karma"  , "KarmaModule.o")
        ,("eval"   , "EvalModule.o")
        ,("type"   , "TypeModule.o")
        ,("dict"   , "DictModule.o")
        ,("quote"  , "QuoteModule.o")
        ,("seen"   , "SeenModule.o")
        ,("dummy"  , "DummyModule.o")
        ,("ghci"   , "GhciModule.o")
        ,("more"   , "MoreModule.o")
        ,("plugs"  , "PlugsModule.o")
        ,("version", "VersionModule.o")
        ,("haddock", "HaddockModule.o")
        ,("cmafihe", "CmafiheModule.o")
        ,("babel"  , "BabelModule.o")
        ,("pl"     , "PlModule.o")
        ,("help"   , "HelpModule.o")]

------------------------------------------------------------------------

getVerbose :: MonadIO m => m Bool
getVerbose = return True

-- for DynamicModule
-- Base, System, Dynamic, More modules shouldn't be dynloaded
getModuleFile         :: MonadIO m => String -> m String
getModuleFile "fact"  = return "FactModule.o"
getModuleFile "hello" = return "HelloModule.o"
getModuleFile "state" = return "StateModule.o"
getModuleFile "topic" = return "TopicModule.o"
getModuleFile "karma" = return "KarmaModule.o"
getModuleFile "eval"  = return "EvalModule.o"
getModuleFile "type"  = return "TypeModule.o"
getModuleFile "dict"  = return "DictModule.o"
getModuleFile "quote" = return "QuoteModule.o"
getModuleFile "seen"  = return "SeenModule.o"
getModuleFile "dummy" = return "DummyModule.o"
getModuleFile "ghci"  = return "GhciModule.o"
getModuleFile "more"  = return "MoreModule.o"
getModuleFile "plugs" = return "PlugsModule.o"
getModuleFile "version" = return "VersionModule.o"
getModuleFile "haddock" = return "HaddockModule.o"
getModuleFile "cmafihe" = return "CmafiheModule.o"
getModuleFile "babel" = return "BabelModule.o"
getModuleFile "pl"    = return "PlModule.o"
getModuleFile "help"    = return "HelpModule.o"
getModuleFile _ = error "unknown module"

data Require = Object String | Package String

--
-- todo, generate this too.
--
--
-- Some of these 'requires' are already hardcoded in
-- DynamicModule.initialise :/
--

getFileRequires "HelpModule.o"  = return [Object "Map.o"]
getFileRequires "FactModule.o"  = return [Package "HToolkit"]
getFileRequires "TopicModule.o" = return [Object "Map.o"]
getFileRequires "StateModule.o" = return [Object "Map.o"]
getFileRequires "SeenModule.o"  = return [Object "Map.o"]
getFileRequires "DictModule.o"  = return [Object "DictModule/DictLookup.o"]
getFileRequires "KarmaModule.o" = return [Object "Map.o"]
getFileRequires "MoreModule.o"  = return [Object "Map.o"]
getFileRequires "EvalModule.o"  = return [Object "EvalModule/LMEngine.o",
                                          Object "Map.o"]
getFileRequires "EvalModule/LMEngine.o" 
                                = return [Object "EvalModule/LMParser.o" 
                                         ,Object "EvalModule/ListTerm.o" 
                                         ,Object "EvalModule/RelTerm.o" 
                                         ,Object "EvalModule/ArithTerm.o" 
                                         ,Object "EvalModule/LangPack.o" 
                                         ,Object "EvalModule/LambdaTerm.o"
                                         ]
getFileRequires "BabelModule.o" = return [Object "BabelBot/BabelFish.o",
                                          Object "MiniHTTP.o"]
getFileRequires "PlModule.o"    = return [Object "PlModule/Transform.o" 
                                         ,Object "PlModule/PrettyPrinter.o"
                                         ,Object "PlModule/Parser.o"
                                         ,Object "PlModule/Common.o"
                                         ,Object "PlModule/Set.o"
                                         ,Object "PlModule/Rules.o"
                                         ]
getFileRequires "HelpModule.o"  = return [Object "Map.o"]

getFileRequires _ = []

------------------------------------------------------------------------
--
-- which modules to dynamically load.
--

#if STATIC

getStartupModules :: [String]
getStartupModules = []

#else

getStartupModules :: [String]
getStartupModules = [
        "dummy", "state","topic","karma","type","seen",
        "dict","quote","eval", "pl","plugs","babel","version",
        "more","help"
        ]

#endif
