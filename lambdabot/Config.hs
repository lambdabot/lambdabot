
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
        admins    :: [String],
        proxy     :: Maybe ([Char], Integer)
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
        proxy           = Nothing,
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

data Require = Object String | Package String

--
-- todo, generate this too.
--
--
-- Some of these 'requires' are already hardcoded in
-- DynamicModule.initialise :/
--

getFileRequires :: String -> [Require]
getFileRequires "DictModule.o"  = [Object "DictModule/DictLookup.o"]
getFileRequires "QuoteModule.o" = [Object "QuoteModule/Fortune.o"]
getFileRequires "EvalModule.o"  = [Object "EvalModule/LMEngine.o"]

getFileRequires "EvalModule/LMEngine.o" =
        [Object "EvalModule/LMParser.o" 
        ,Object "EvalModule/ListTerm.o" 
        ,Object "EvalModule/RelTerm.o" 
        ,Object "EvalModule/ArithTerm.o" 
        ,Object "EvalModule/LangPack.o" 
        ,Object "EvalModule/LambdaTerm.o" ]

getFileRequires "BabelModule.o" = 
        [Object "BabelBot/BabelFish.o", Object "MiniHTTP.o"]

getFileRequires "PlModule.o"    =
        [Object "PlModule/Transform.o" 
        ,Object "PlModule/PrettyPrinter.o"
        ,Object "PlModule/Parser.o"
        ,Object "PlModule/Common.o"
        ,Object "PlModule/Set.o"
        ,Object "PlModule/Rules.o" ]

getFileRequires "HaddockModule.o" = [ 
        Object "haddock/HaddockHtml.o",
        Object "haddock/HaddockVersion.o",
        Object "haddock/HaddockTypes.o",
        Object "haddock/HsSyn.o",
        Object "haddock/Binary.o",
        Object "haddock/FastMutInt.o",
        Object "haddock/Html.o",
        Object "haddock/HaddockHH.o",
        Object "haddock/HaddockModuleTree.o",
        Object "haddock/Digraph.o",
        Object "haddock/BlockTable.o",
        Object "haddock/HaddockUtil.o"]

getFileRequires _ = []
