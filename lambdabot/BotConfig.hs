module BotConfig where
import Control.Monad.Trans

getMyname :: MonadIO m => m String
getMyname = return "lambdabot"

getMyuserinfo :: MonadIO m => m String
getMyuserinfo = return "Lambda Robots - 100% Loyal"

getHost :: MonadIO m => m String
getHost = return "irc.eu.freenode.net"

getPort :: MonadIO m => m String
getPort = return "6667"

getAutojoins :: MonadIO m => m [String]
getAutojoins = return ["#haskell"]

getAdmins :: MonadIO m => m [String]
getAdmins = return ["Pseudonym","shapr","pesco","Riastradh","Darius",
                     "tmoertel","delYsid","polli","Heffalump","Igloo",
                     "Marvin--","o3","o3_","phubuh","ddarius","bringert","dons"]

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

getFileRequires                 :: MonadIO m => String -> m [Require]
getFileRequires "HaddockModule.o" = return [
                                            Object "Map.o",
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

getFileRequires "QuoteModule.o" = return [Object "QuoteModule/Fortune.o",
                                          Object "Map.o"]

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
                                         ,Package "parsec"
                                         ]
getFileRequires "GHCiModule.o"  = return [Object "Shell.o", Object "Map.o"]
getFileRequires "BabelModule.o" = return [Object "BabelBot/BabelFish.o",
                                          Object "MiniHTTP.o"]
getFileRequires "PlModule.o"    = return [Object "PlModule/Transform.o" 
                                         ,Object "PlModule/PrettyPrinter.o"
                                         ,Object "PlModule/Parser.o"
                                         ,Object "PlModule/Common.o"
                                         ,Object "PlModule/Set.o"
                                         ,Object "PlModule/Rules.o"
                                         ,Package "parsec"
                                         ]
getFileRequires "HelpModule.o"  = return [Object "Map.o"]

getFileRequires _ = return []

getStartupModules :: MonadIO m => m [String]
getStartupModules = return [
        "dummy", "state","topic","karma","type","seen",
        "dict","quote","eval", "pl","plugs","babel","version",
        "more","help"
        ] --,"fact","haddock"]

-- for the MoreModule, how many lines to show at a time
getMaxLines :: MonadIO m => m Int
getMaxLines = return 7
