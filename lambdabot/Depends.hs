-- Generated via "make dynamic-depends"
module Depends where

data Require = Object String | Package String

#if __GLASGOW_HASKELL__ < 604

corePlugins :: [String]
corePlugins = ["Map","Util","IRC","Config","MonadException","ExceptionError","ErrorUtils","DeepSeq"]

reqPackages :: [String]
reqPackages = ["base","haskell98","lang","parsec","network","unix","posix"]

getFileRequires :: String -> [Require]

getFileRequires "DictModule.o" = [ Object "DictModule/DictLookup.o" ]
getFileRequires "PlugsModule.o" = [ Object "PosixCompat.o" ]
getFileRequires "QuoteModule.o" = [ Object "QuoteModule/Fortune.o" ]
getFileRequires "TypeModule.o" = [ Object "PosixCompat.o" ]

getFileRequires "BabelModule.o" = [
    Object "PosixCompat.o" ,
    Object "BabelBot/BabelFish.o" ,
    Object "MiniHTTP.o" ]

getFileRequires "DynamicModule.o" = [
    Object "RuntimeLoader.o" ,
    Object "Depends.o" ,
    Object "Modules.o" ,
    Object "ParsePkgConf.o" ]

getFileRequires "EvalModule.o" = [
    Object "EvalModule/LMEngine.o" ,
    Object "EvalModule/LMParser.o" ,
    Object "EvalModule/ListTerm.o" ,
    Object "EvalModule/RelTerm.o" ,
    Object "EvalModule/LambdaTerm.o" ,
    Object "EvalModule/ArithTerm.o" ,
    Object "EvalModule/LangPack.o" ]

getFileRequires "GoogleModule.o" = [ Object "MiniHTTP.o" ]
getFileRequires "VixenModule.o" = [ Object "Vixen.o" ]
getFileRequires "PlModule.o" = [
    Object "PlModule/Transform.o" ,
    Object "PlModule/PrettyPrinter.o" ,
    Object "PlModule/Parser.o" ,
    Object "PlModule/Common.o" ,
    Object "PlModule/Set.o" ,
    Object "PlModule/Rules.o" ]

getFileRequires _ = []

#else

corePlugins :: [String]
corePlugins = ["Config","DeepSeq","ErrorUtils","ExceptionError","IRC","Map","MonadException","Util"]

reqPackages :: [String]
reqPackages = ["base","haskell98","mtl","parsec","network","unix"]

getFileRequires :: String -> [Require]

getFileRequires "BabelModule.o" = [
    Object "BabelBot/BabelFish.o" ,
    Object "MiniHTTP.o" ,
    Object "PosixCompat.o" ]

getFileRequires "DictModule.o" = [ Object "DictModule/DictLookup.o" ]
getFileRequires "DynamicModule.o" = [
    Object "Modules.o" ,
    Object "ParsePkgConf.o" ,
    Object "RuntimeLoader.o" ]

getFileRequires "EvalModule.o" = [
    Object "EvalModule/ArithTerm.o" ,
    Object "EvalModule/LMEngine.o" ,
    Object "EvalModule/LMParser.o" ,
    Object "EvalModule/LambdaTerm.o" ,
    Object "EvalModule/LangPack.o" ,
    Object "EvalModule/ListTerm.o" ,
    Object "EvalModule/RelTerm.o" ]

getFileRequires "GoogleModule.o" = [ Object "MiniHTTP.o" ]
getFileRequires "VixenModule.o" = [ Object "Vixen.o" ]
getFileRequires "PlModule.o" = [
    Object "PlModule/Common.o" ,
    Object "PlModule/Parser.o" ,
    Object "PlModule/PrettyPrinter.o" ,
    Object "PlModule/Rules.o" ,
    Object "PlModule/Set.o" ,
    Object "PlModule/Transform.o" ]

getFileRequires "PlugsModule.o" = [ Object "PosixCompat.o" ]
getFileRequires "QuoteModule.o" = [ Object "QuoteModule/Fortune.o" ]
getFileRequires "TypeModule.o" = [ Object "PosixCompat.o" ]

getFileRequires _ = []

#endif
