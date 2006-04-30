--
-- | Lambda calculus interpreter
--
module Plugin.Lambda (theModule) where

import Plugin
import Plugin.Lambda.LMEngine (evaluate, define, resume, Environment)

import qualified Data.Map as M
import Lib.Util (mapMaybeMap)

import Data.Dynamic                     (Dynamic)

import qualified Data.ByteString.Char8 as P

PLUGIN Eval

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

    moduleSerialize _ = Just $ Serial {
              serialize = Just . (\(fuel,_,defns) -> 
                P.pack . unlines $ show fuel: map show (M.toList defns)) . global,
              deserialize = fmap (mkGlobalPrivate maxPrivate) .  loadDefinitions . P.unpack
           }

    moduleHelp _ s = case s of
        "lambda"         -> "lambda <expr>. Evaluate the lambda calculus expression, <expr>"
        "define"         -> "define <name> <expr>. Define name to be expr"
        "get-definition" -> "get-definition <name>. Get the expression defining name"
        "definitions"    -> "definitions <prefix>. Get the definitions starting with prefix"
        "del-definition" -> "del-definition <name>. Delete name"
        "set-fuel"       -> "set-fuel <ticks>. How many ticks before @lambda runs out of fuel"
        "resume"         -> "resume. Continue an expression that has run out of fuel"

    moduleCmds   _ = ["lambda","define","get-definition","definitions","resume"]
    modulePrivs  _ = ["set-fuel","del-definition"]

    process      _ _ target cmd rest = do
       let writeRes = writePS target
       (fuel, env, defns) <- readGS
       res <- readPS target
       case cmd of
        "lambda" -> do 
             let r_or_s = evaluate rest env fuel
             case r_or_s of
                Right s -> writeRes Nothing   >> return [s]
                Left nr -> writeRes (Just nr) >> return [outOfFuelMsg]

        "define" -> 
            let rslt = define defn
                (nm,defn) = break (' '==) rest 
            in case rslt of
                    Left s  -> return [s]
                    Right v -> do
                      withGS $ \(fuel', env', defns') writer -> do
                          writer (fuel', M.insert nm v env', 
                                         M.insert nm defn defns')
                      return [nm ++ " defined"]

        "definitions" -> 
            let nm = M.keys defns 
            in return . (:[]) $ if null rest 
                then unlines $ map show $
                                       groupBy (\(x:_) (y:_) -> x == y) $ nm
                else show [x | x <- sort nm, rest `isPrefixOf` x]

        "get-definition" -> 
            let defn = M.lookup rest defns 
                out = maybe (rest++" not defined") ((rest++" =")++) defn
            in return [out]

        "set-fuel" -> case readM rest of
            Nothing -> return ["Not a number"]
            Just x  -> if x > 0 && x <= maxFuel 
                        then do writeGS (x,env,defns)
                                return ["Fuel set to "++show x]
                        else return ["Can't set fuel above "++show maxFuel]

        "del-definition" -> case words rest of
            (d:_) -> do
                withGS $ \(fuel',env',defns') writer -> 
                    writer (fuel', M.delete d env', M.delete d defns')
                return [d++" removed"]
            _ -> return []

        "resume" -> case res of
            Nothing -> return []
            Just r -> case resume r fuel of
                    Left nr -> do
                        writeRes $ Just nr
                        return [outOfFuelMsg]
                    Right s -> do
                        writeRes Nothing
                        return [s]

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
      eMap = mapMaybeMap ((const Nothing `either` \x -> Just $! x) . define) $ dMap

      keys = M.keys eMap
  
  fuel `seq` eMap `seq` dMap `seq` return
                (fuel,
                 eMap,
                 M.filterWithKey (\k _ -> k `elem` keys) dMap) 

