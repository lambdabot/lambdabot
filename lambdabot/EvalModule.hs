{-# OPTIONS -fallow-overlapping-instances #-}

module EvalModule (evalModule,theModule) where

import qualified Map as M

-- 	$Id: EvalModule.hs,v 1.1 2003/07/29 13:41:48 eleganesh Exp $
import IRC
import Util

import Data.IORef
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

instance Module EvalModule where
    moduleName   _ = return "eval"
    moduleSticky _ = False
    commands     _ = return ["eval","define","get-definition","definitions",
                             "del-definition","dump","set-fuel","resume"]
    moduleInit   _ = do
        r <- liftIO $ catch loadDefinitions
                    (io_or_pm $
                     newIORef $
                     ModuleState (initFuel,
                                  Nothing :: Maybe Dynamic,
                                  initEnv,
                                  initDefns))
        modify (\s -> s { ircModuleState = M.insert  "eval" r (ircModuleState s)})
    process      _ msg target cmd rest = do
       Just ref <- gets (\s -> M.lookup  "eval" (ircModuleState s))
       ms <- liftIO $ readIORef ref
       let (fuel, res, env, defns) = stripMS ms
       case cmd of
            "eval" -> do let r_or_s = evaluate rest env fuel
                         case r_or_s of
                            Right s -> ircPrivmsg target s
                            Left nr -> do liftIO $ writeIORef ref $
                                                    ModuleState (fuel,
                                                                 Just nr,
                                                                 env,
                                                                 defns)
                                          ircPrivmsg target outOfFuelMsg
            "define" -> let rslt = define defn
                            (name,defn) = break (' '==) rest in
                          case rslt of
                               Left s -> ircPrivmsg target s
                               Right v -> do
                                  liftIO $ writeIORef ref $
                                    ModuleState (fuel,
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
                                 do liftIO $ writeIORef ref $
                                                ModuleState (x,res,env,defns)
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
                                    liftIO $ writeIORef ref $
                                                ModuleState (fuel,
                                                             res,
                                                             delFromFM env d,
                                                             delFromFM defns d)
                                    ircPrivmsg target $ d++" removed"
            "resume" -> case res of
                            Nothing -> return ()
                            Just r -> case resume r fuel of
                                        Left nr -> do
                                            liftIO $ writeIORef ref $
                                                        ModuleState (fuel,
                                                                     Just nr,
                                                                     env,
                                                                     defns)
                                            ircPrivmsg target outOfFuelMsg
                                        Right s -> do
                                            liftIO $ writeIORef ref $
                                                        ModuleState (fuel,
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
loadDefinitions :: IO (IORef ModuleState)
loadDefinitions = do
    s <- readFile definitionsFile
    let ((fuel,rest):_) = reads s :: [(Int,String)]
        the_d_FM = listToFM $ (read rest :: [(String,String)])
        the_e_FM = mapFM (\_ (Right v) -> v) $
                   filterFM (const $ either (const False) (flip seq True)) $
                   mapFM (const $ define) the_d_FM :: Environment
        ks = keysFM the_e_FM
    fuel `seq` the_e_FM `seq` the_d_FM `seq`
        (newIORef $ ModuleState (fuel,
                                 Nothing :: Maybe Dynamic,
                                 the_e_FM,
                                 filterFM (\k _ -> k `elem` ks) the_d_FM))
