
module EvalModule (theModule) where

import EvalModule.LMEngine (evaluate, define, resume, Environment)

import IRC
import Util                             (Serializer(..), readM)
import qualified Map as M

import Data.Dynamic                     (Dynamic)
import Data.List                        (groupBy,sort,isPrefixOf)
import Data.Maybe                       (catMaybes)
import Control.Monad.State

-- TODO: clear continuation IORef after -every- @eval?

newtype EvalModule = EvalModule ()

theModule :: MODULE
theModule = MODULE $ EvalModule ()

initFuel :: Int
initFuel = 1000

initEnv :: Environment
initEnv = M.empty

initDefns :: M.Map String String
initDefns = M.empty

outOfFuelMsg :: [Char]
outOfFuelMsg = "out of fuel - use @resume to continue"

type EvalState = (Int, Maybe Dynamic, Environment, M.Map String String)

instance Module EvalModule EvalState where
    moduleName   _ = "eval"

    moduleDefState _ = return (initFuel, Nothing, initEnv, initDefns)
    moduleSerialize _ = Just $ Serializer {
      serialize = \(fuel,_,_,defns) -> 
        unlines $ show fuel: map show (M.toList defns),
      deSerialize = loadDefinitions
    }
    
    moduleHelp   _ "eval" = return "@eval expr - evaluate the lambda calculus expression, expr"
    moduleHelp   _ "define" = return "@define name expr - define name to be expr"
    moduleHelp   _ "get-definition" = return "@get-definition name - get the expression defining name"                               
    moduleHelp   _ "definitions" = return "@definitions [prefix] - get the definitions starting with prefix"
    moduleHelp   _ "del-definition" = return "@del-definition name - delete name"
    moduleHelp   _ "set-fuel" = return "@set-fuel ticks - how many ticks before @eval runs out of fuel"
    moduleHelp   _ "resume" = return "@resume - continue an expression that has run out of fuel"
    moduleHelp   _ cmd = return $ "EvalModule: don't know command "++cmd
    moduleCmds   _ = return ["eval","define","get-definition","definitions",
                             "del-definition","set-fuel","resume"]

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
                                   M.insert name v env,
                                   M.insert name defn defns)
                          ircPrivmsg target (name ++ " defined")

            "definitions" -> 
                let names = M.keys defns 
                in if null rest 
                    then ircPrivmsg target (unlines $ map show $
                                           groupBy (\(x:_) (y:_) -> x == y) $ names)
                    else ircPrivmsg target $ show 
                                [x | x <- sort names, rest `isPrefixOf` x]

            "get-definition" -> 
                let defn = M.lookup rest defns 
                    out = maybe (rest++" not defined") ((rest++" =")++) defn
                in ircPrivmsg target out

            "set-fuel" -> case reads rest :: [(Int,String)] of
                [] -> checkPrivs msg target (ircPrivmsg target "not a number")
                ((x,_):_) -> checkPrivs msg target $ do 
                                writeMS (x,res,env,defns)
                                ircPrivmsg target $ "fuel set to "++show x

            "del-definition" -> case words rest of
                [] -> return ()
                (d:_) -> checkPrivs msg target $ do
                    writeMS (fuel,
                             res,
                             M.delete d env,
                             M.delete d defns)
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


--
-- this is so ugly (at least it's only init)
--
-- This stuff is slow
--
loadDefinitions :: String -> Maybe EvalState
loadDefinitions s = do
  -- grab fuel val from definitions.
  fuel':rest <- return $ lines s
  fuel <- readM fuel'
  -- rest is a list of paris of ids and rhs defins
  let rests = catMaybes $ map readM rest
  -- parse the lot. :/
  let the_d_FM = M.fromList $! rests
      (the_e_FM :: Environment) = 
        M.mapWithKey (\_ (Right v) -> v) $
           M.filterWithKey (const $ either (const False) (flip seq True)) $
               M.mapWithKey (const $ define) the_d_FM

      keys = M.keys the_e_FM
  
  fuel `seq` the_e_FM `seq` the_d_FM `seq` return
                (fuel,
                 Nothing,
                 the_e_FM,
                 M.filterWithKey (\k _ -> k `elem` keys) the_d_FM) 

