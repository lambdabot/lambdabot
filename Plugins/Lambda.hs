--
-- | Lambda calculus interpreter
--
module Plugins.Lambda (theModule) where

import Plugins.Lambda.LMEngine (evaluate, define, resume, Environment)

import Lambdabot
import Util                             (Serializer(..), readM)
import qualified Map as M

import Data.Dynamic                     (Dynamic)
import Data.List                        (groupBy,sort,isPrefixOf)
import Data.Maybe                       (mapMaybe)

-- TODO: clear continuation IORef after -every- @eval?

newtype EvalModule = EvalModule ()

theModule :: MODULE
theModule = MODULE $ EvalModule ()

initFuel, maxFuel :: Int
initFuel = 1000
maxFuel  = 500000 -- ~ 200M mem usage for "sum $ enumFrom 1"

maxPrivate :: Int
maxPrivate = 10

initEnv :: Environment
initEnv = M.empty

initDefns :: M.Map String String
initDefns = M.empty

outOfFuelMsg :: [Char]
outOfFuelMsg = "out of fuel - use @resume to continue"

type EvalGlobal = (Int, Environment, M.Map String String)
type EvalState = GlobalPrivate EvalGlobal Dynamic 

instance Module EvalModule EvalState where
    moduleDefState _ = return $ mkGlobalPrivate (maxPrivate) 
      (initFuel, initEnv, initDefns)
    moduleSerialize _ = Just $ Serializer {
      serialize = Just . (\(fuel,_,defns) -> 
        unlines $ show fuel: map show (M.toList defns)) . global,
      deSerialize = fmap (mkGlobalPrivate maxPrivate) . loadDefinitions
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
       let writeRes = writePS target
       (fuel, env, defns) <- readGS
       res <- readPS target
       case cmd of
            "eval" -> do 
                 let r_or_s = evaluate rest env fuel
                 case r_or_s of
                    Right s -> do writeRes Nothing
                                  ircPrivmsg target s
                    Left nr -> do writeRes $ Just nr
                                  ircPrivmsg target outOfFuelMsg

            "define" -> 
                let rslt = define defn
                    (name,defn) = break (' '==) rest 
                in case rslt of
                        Left s  -> ircPrivmsg target s
                        Right v -> do
                          writeGS (fuel, 
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

            "set-fuel" -> case readM rest of
                Nothing -> checkPrivs msg target (ircPrivmsg target "not a number")
                Just x  -> checkPrivs msg target $ case x > 0 && x <= maxFuel of
                    True  -> do 
                        writeGS (x,env,defns)
                        ircPrivmsg target $ "fuel set to "++show x
                    False -> ircPrivmsg target $ "can't set fuel above "++show maxFuel

            "del-definition" -> case words rest of
                [] -> return ()
                (d:_) -> checkPrivs msg target $ do
                    writeGS (fuel,
                             M.delete d env,
                             M.delete d defns)
                    ircPrivmsg target $ d++" removed"

            "resume" -> case res of
                Nothing -> return ()
                Just r -> case resume r fuel of
                        Left nr -> do
                            writeRes $ Just nr
                            ircPrivmsg target outOfFuelMsg
                        Right s -> do
                            writeRes Nothing
                            ircPrivmsg target s

            _       -> ircPrivmsg target ("unknown command: "++cmd)


--
-- this is so ugly (at least it's only init)
--
-- This stuff is slow
--
loadDefinitions :: String -> Maybe EvalGlobal
loadDefinitions s = do
  -- grab fuel val from definitions.
  fuel':rest <- return $ lines s
  fuel <- readM fuel'
  -- rest is a list of paris of ids and rhs defins
  let rests = mapMaybe readM rest
  -- parse the lot. :/
  let dMap = M.fromList $! rests
      eMap = M.mapMaybe ((const Nothing `either` \x -> Just $! x) . define) $ dMap

      keys = M.keys eMap
  
  fuel `seq` eMap `seq` dMap `seq` return
                (fuel,
                 eMap,
                 M.filterWithKey (\k _ -> k `elem` keys) dMap) 

