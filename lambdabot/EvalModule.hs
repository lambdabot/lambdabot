{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module EvalModule (evalModule,theModule) where

-- 	$Id: EvalModule.hs,v 1.1 2003/07/29 13:41:48 eleganesh Exp $
import IRC
import Util

import Control.Monad.State
import Data.FiniteMap
import Data.Dynamic
import Data.List (groupBy,sort,isPrefixOf)

import EvalModule.LMEngine (evaluate, define, resume, Environment)

import Prelude hiding (catch)
import Control.Exception (throw, catch, Exception(..), ioErrors, handleJust)

-- TODO: clear continuation IORef after -every- @eval?

newtype EvalModule = EvalModule ()

theModule :: MODULE
theModule = MODULE evalModule

evalModule :: EvalModule
evalModule = EvalModule ()

initFuel :: Int
initFuel = 1000

initEnv :: Environment
initEnv = emptyFM

initDefns :: FiniteMap String String
initDefns = emptyFM

definitionsFile :: [Char]
definitionsFile = "definitions"

outOfFuelMsg :: [Char]
outOfFuelMsg = "out of fuel - use @resume to continue"

type EvalState = (Int, Maybe Dynamic, Environment, FiniteMap String String)

instance Module EvalModule EvalState where
    moduleName   _ = return "eval"
    moduleHelp   _ "eval" = return "@eval expr - evaluate the lambda calculus expression, expr"
    moduleHelp   _ "define" = return "@define name expr - define name to be expr"
    moduleHelp   _ "get-definition" = return "@get-definition name - get the expression defining name"                               
    moduleHelp   _ "definitions" = return "@definitions [prefix] - get the definitions starting with prefix"
    moduleHelp   _ "del-definition" = return "@del-definition name - delete name"
    moduleHelp   _ "dump" = return "@dump - dump definitions to disk"
    moduleHelp   _ "set-fuel" = return "@set-fuel ticks - how many ticks before @eval runs out of fuel"
    moduleHelp   _ "resume" = return "@resume - continue an expression that has run out of fuel"
    moduleHelp   _ cmd = return $ "EvalModule: don't know command "++cmd
    moduleSticky _ = False
    commands     _ = return ["eval","define","get-definition","definitions",
                             "del-definition","dump","set-fuel","resume"]
    moduleInit   _ = do
        r <- liftIO $ catch loadDefinitions
                    (io_or_pm $
                     return (initFuel,
                             Nothing :: Maybe Dynamic,
                             initEnv,
                             initDefns))
        writeMS r
    process      _ msg target cmd rest = do
       (fuel, res, env, defns) <- readMS
       case cmd of
            "eval" -> do let r_or_s = evaluate rest env fuel
                         case r_or_s of
                            Right s -> ircPrivmsg target s
                            Left nr -> do writeMS (fuel,
                                                   Just nr,
                                                   env,
                                                   defns)
                                          ircPrivmsg target outOfFuelMsg
            "define" -> let rslt = define defn
                            (name,defn) = break (' '==) rest in
                          case rslt of
                               Left s -> ircPrivmsg target s
                               Right v -> do
                                  writeMS (fuel,
                                           res,
                                           addToFM env name v,
                                           addToFM defns name defn)
                                  ircPrivmsg target (name ++ " defined")
            "definitions" -> let names = keysFM defns in
                                    if null rest then
                                        ircPrivmsg target (unlines $
                                                           map show $
                                                           groupBy (\(x:_) (y:_) -> x == y) $
                                                           names)
                                    else ircPrivmsg target $ show
                                            [x | x <- sort names, 
                                                 rest `isPrefixOf` x]
            "get-definition" -> let defn = lookupFM defns rest
                                    out = maybe (rest++" not defined") ((rest++" =")++) defn
                                in ircPrivmsg target out
            "set-fuel" -> do case reads rest :: [(Int,String)] of
                                [] -> checkPrivs msg target (ircPrivmsg target "not a number")
                                ((x,_):_) -> checkPrivs msg target $
                                 do writeMS (x,res,env,defns)
                                    ircPrivmsg target $ "fuel set to "++show x
            "dump" -> checkPrivs msg target $ do
                      mex <- liftIO $ handleJust ioErrors (return . Just) $
                              do writeFile definitionsFile
                                   (show fuel ++ '\n': (show $ fmToList defns))
                                 return Nothing
                      case mex of
                          Nothing -> ircPrivmsg target "dumped"
                          Just ex -> liftIO $ debugStrLn (show ex)
            "del-definition" -> case words rest of
                                 [] -> return ()
                                 (d:_) -> checkPrivs msg target $ do
                                    writeMS (fuel,
                                             res,
                                             delFromFM env d,
                                             delFromFM defns d)
                                    ircPrivmsg target $ d++" removed"
            "resume" -> case res of
                            Nothing -> return ()
                            Just r -> case resume r fuel of
                                        Left nr -> do
                                            writeMS (fuel,
                                                     Just nr,
                                                     env,
                                                     defns)
                                            ircPrivmsg target outOfFuelMsg
                                        Right s -> do
                                            writeMS $ (fuel,
                                                       Nothing :: Maybe Dynamic,
                                                       env,
                                                       defns)
                                            ircPrivmsg target s
            _       -> ircPrivmsg target ("unknown command: "++cmd)


io_or_pm :: a -> Exception -> a
io_or_pm c (PatternMatchFail _) = c
io_or_pm c (IOException _) = c
io_or_pm _ e = throw e -- ioError e (throw should work for both 5.04/5.05)

-- this is so ugly (at least it's only init)
loadDefinitions :: IO EvalState
loadDefinitions = do
    s <- readFile definitionsFile
    let ((fuel,rest):_) = reads s :: [(Int,String)]
        the_d_FM = listToFM $ (read rest :: [(String,String)])
        the_e_FM = mapFM (\_ (Right v) -> v) $
                   filterFM (const $ either (const False) (flip seq True)) $
                   mapFM (const $ define) the_d_FM :: Environment
        ks = keysFM the_e_FM
    fuel `seq` the_e_FM `seq` the_d_FM `seq`
        (return (fuel,
                 Nothing,
                 the_e_FM,
                 filterFM (\k _ -> k `elem` ks) the_d_FM))
