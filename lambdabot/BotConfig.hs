module BotConfig where
import Control.Monad.Trans

getMyname :: MonadIO m => m String
getMyname = return "lambdabot"

getMyuserinfo :: MonadIO m => m String
getMyuserinfo = return "Lambda"

getHost :: MonadIO m => m String
getHost = return "irc.eu.freenode.net"

getPort :: MonadIO m => m String
getPort = return "6667"

getAutojoins :: MonadIO m => m [String]
getAutojoins = return ["#joy","#haskell"]

getAdmins :: MonadIO m => m [String]
getAdmins = return ["Pseudonym","shapr","pesco","Riastradh","Darius",
                    "tmoertel","delYsid","polli","Heffalump","Igloo",
                    "Marvin--","o3","o3_","phubuh","ddarius","bringert"]

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
getModuleFile "haddock"  = return "HaddockModule.o"
getModuleFile "cmafihe"  = return "CmafiheModule.o"
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
getFileRequires "QuoteModule.o" = return [Object "QuoteModule/Yow.o",
                                          Object "QuoteModule/Fortune.o"]
getFileRequires "FactModule.o"  = return [Package "HToolkit"]
getFileRequires "TopicModule.o" = return [Object "Map.o"]
getFileRequires "StateModule.o" = return [Object "Map.o"]
getFileRequires "SeenModule.o"  = return [Object "Map.o"]
getFileRequires "DictModule.o"  = return [Object "DictModule/DictLookup.o"]
getFileRequires "KarmaModule.o"  = return [Object "Map.o"]
getFileRequires "EvalModule.o"  = return [Object "EvalModule/LMEngine.o",
                                          Object "Map.o"]
getFileRequires "EvalModule/LMEngine.o" 
                                = return [Object "EvalModule/LMParser.o",
                                          Object "EvalModule/ListTerm.o",
                                          Object "EvalModule/RelTerm.o",
                                          Object "EvalModule/ArithTerm.o",
                                          Object "EvalModule/LangPack.o",
                                          Object "EvalModule/LambdaTerm.o"
                                         ]
getFileRequires "GHCiModule.o"  = return [Object "Shell.o", Object "Map.o"]
getFileRequires "PlugsModule.o" = return [Object "PlugsModule/RunPlugs.o"]

getFileRequires _ = return []

getStartupModules :: MonadIO m => m [String]
getStartupModules = return ["dummy","hello","state","topic","karma","more","haddock","cmafihe",
                            "type","seen","dict","quote","eval","ghci"] --,"fact"]

-- for the MoreModule, how many lines to show at a time
getMaxLines :: MonadIO m => m Int
getMaxLines = return 9
