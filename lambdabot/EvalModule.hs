-- 	$Id: EvalModule.hs,v 1.1 2003/07/29 13:41:48 eleganesh Exp $

module EvalModule (evalModule,theModule) where

import EvalModule.LMEngine (evaluate, define, resume, Environment)

import IRC
import Util
import Map (Map)
import qualified Map as Map hiding (Map)

import Data.Dynamic
import Data.List                        (groupBy,sort,isPrefixOf)
import Control.Monad.State
import Control.Exception                (Exception(..))
import qualified Control.Exception as C (throw, catch, ioErrors, handleJust)

-- TODO: clear continuation IORef after -every- @eval?

newtype EvalModule = EvalModule ()

theModule :: MODULE
theModule = MODULE evalModule

evalModule :: EvalModule
evalModule = EvalModule ()

initFuel :: Int
initFuel = 1000

initEnv :: Environment
initEnv = Map.empty

initDefns :: Map String String
initDefns = Map.empty

definitionsFile :: [Char]
definitionsFile = "definitions"

outOfFuelMsg :: [Char]
outOfFuelMsg = "out of fuel - use @resume to continue"

type EvalState = (Int, Maybe Dynamic, Environment, Map String String)

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
        r <- liftIO $ C.catch loadDefinitions
                    (io_or_pm $
                     return (initFuel,
                             Nothing :: Maybe Dynamic,
                             initEnv,
                             initDefns))
        writeMS r
    process      _ msg target cmd rest = do
       (fuel, res, env, defns) <- readMS
       case cmd of
            "eval" -> do 
                 let r_or_s = evaluate rest env fuel
                 case r_or_s of
                    Right s -> ircPrivmsg target s
                    Left nr -> do writeMS (fuel,
                                           Just nr,
                                           env,
                                           defns)
                                  ircPrivmsg target outOfFuelMsg

            "define" -> 
                let rslt = define defn
                    (name,defn) = break (' '==) rest 
                in case rslt of
                        Left s  -> ircPrivmsg target s
                        Right v -> do
                          writeMS (fuel, res,
                                   Map.insert name v env,
                                   Map.insert name defn defns)
                          ircPrivmsg target (name ++ " defined")

            "definitions" -> 
                let names = Map.keys defns 
                in if null rest 
                    then ircPrivmsg target (unlines $ map show $
                                           groupBy (\(x:_) (y:_) -> x == y) $ names)
                    else ircPrivmsg target $ show 
                                [x | x <- sort names, rest `isPrefixOf` x]

            "get-definition" -> 
                let defn = Map.lookup rest defns 
                    out = maybe (rest++" not defined") ((rest++" =")++) defn
                in ircPrivmsg target out

            "set-fuel" -> case reads rest :: [(Int,String)] of
                [] -> checkPrivs msg target (ircPrivmsg target "not a number")
                ((x,_):_) -> checkPrivs msg target $ do 
                                writeMS (x,res,env,defns)
                                ircPrivmsg target $ "fuel set to "++show x

            "dump" -> checkPrivs msg target $ do
                      mex <- liftIO $ C.handleJust C.ioErrors (return . Just) $
                              do writeFile definitionsFile
                                   (show fuel ++ '\n': (show $ Map.toList defns))
                                 return Nothing
                      case mex of
                          Nothing -> ircPrivmsg target "dumped"
                          Just ex -> liftIO $ debugStrLn (show ex)

            "del-definition" -> case words rest of
                [] -> return ()
                (d:_) -> checkPrivs msg target $ do
                    writeMS (fuel,
                             res,
                             Map.delete d env,
                             Map.delete d defns)
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
io_or_pm _ e = C.throw e -- ioError e (throw should work for both 5.04/5.05)

--
-- this is so ugly (at least it's only init)
--
-- This stuff is slow
--
loadDefinitions :: IO EvalState
loadDefinitions = do
    s <- readFile definitionsFile

    -- grab fuel val from definitions.
    let ((fuel,rest):_) = reads s :: [(Int,String)]

    -- rest is a list of paris of ids and rhs defins
    let rests    = read rest :: [(String,String)]

    -- parse the lot. :/
    let the_d_FM = Map.fromList $! rests
        (the_e_FM :: Environment) = 
          Map.mapWithKey (\_ (Right v) -> v) $
             Map.filterWithKey (const $ either (const False) (flip seq True)) $
                 Map.mapWithKey (const $ define) the_d_FM

    let keys = Map.keys the_e_FM

    fuel `seq` the_e_FM `seq` the_d_FM `seq`
        (return (fuel,
                 Nothing,
                 the_e_FM,
                 Map.filterWithKey (\k _ -> k `elem` keys) the_d_FM))

