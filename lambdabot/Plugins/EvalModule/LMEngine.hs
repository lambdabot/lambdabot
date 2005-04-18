{-# OPTIONS -fallow-overlapping-instances #-}
-- until we fix it properly 

module Plugins.EvalModule.LMEngine (
     evaluate,
     define,
     Environment,
     resume
  ) where

import Map (Map)
import qualified Map as Map hiding (Map)

import Plugins.EvalModule.LangPack
import Plugins.EvalModule.ArithTerm
import Plugins.EvalModule.LambdaTerm
import Plugins.EvalModule.RelTerm
import Plugins.EvalModule.ListTerm
import Plugins.EvalModule.LMParser

import Data.Maybe           (fromJust)
import Data.Dynamic
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Cont
import Control.Monad.Identity

import Text.ParserCombinators.Parsec.Error (ParseError)

-- temporarily
-- import Maybe
-- import Debug.Trace

------------------------------------------------------------------------

-- TODO: replace showTerm

evaluate :: String -> Environment -> Int -> Either Dynamic String
evaluate s env fuel = eval env fuel $ parseTerm s

define :: String -> Either String (EvalMonad Value)
define = either (Left . show) (Right . fold phi) . parseTerm

type Environment = Map String (EvalMonad Value)
type Value = Dynamic
type Result = Either Dynamic (Either String String)

newtype EvalMonad a = EM { runEM :: (StateT (IState EvalMonad)
                                    (ReaderT Environment
                                    (ErrorT String
                                    (Cont Result)))) a }

phi :: Term (EvalMonad Value) -> EvalMonad Value
phi (ArithT x) = phiArith x
phi (LambdaT x) = phiLambda x
phi (RelT x) = phiRel x
phi (ListT x) = phiList x

run :: EvalMonad Value -> Environment -> Int -> Result
run m env fuel = flip runCont Right $
                 runErrorT $
                 runReaderT (evalStateT (runEM (m >>= showDyn)) (0,Map.empty,fuel)) env

eval :: Environment -> Int -> Either ParseError (Fix Term) -> Either Dynamic String
eval env fuel = either (Right . show) doit
    where doit x = -- trace (x `seq` showTerm x) $
                    res_or_str $ run (do env' <- startup
                                         local (const $ Map.fromList env')
                                               (fold phi x))
                                     Map.empty fuel
          startup = Map.foldWithKey mkThunk (return []) env
          mkThunk k c cs = do env' <- cs
                              rr <- thunkify c
                              return ((k,rr):env')


res_or_str :: Either a (Either b b) -> Either a b
res_or_str = either Left (either Right Right)


-- TODO: get rid of fromJust
resume :: Dynamic -> Int -> Either Dynamic String
resume r i = res_or_str $ fromJust (fromDynamic r) i

showDyn :: Dynamic -> EvalMonad String
showDyn d = case (fromDynamic d :: Maybe Integer) of {
                Just i -> return $ show i;
                _      ->
            case (fromDynamic d :: Maybe Bool) of {
                Just b -> return $ show b;
                _      ->
            case (fromDynamic d :: Maybe Char) of {
                Just c -> return [c];
                _      ->
            case (fromDynamic d :: Maybe ()) of {
                Just _ -> return "[]";
                _      ->
            case (fromDynamic d :: Maybe (EvalMonad Dynamic, EvalMonad Dynamic)) of {
                Just l -> (do b <- foldList (\x y -> do
                                                b <- isCharacter x
                                                liftM (b&&) y)
                                            (return True) (return d)
                              if b then showListDyn "" l
                                   else do x <- showListDyn ", " l
                                           return $ "["++x++"]");
                _      -> return $ show d;
            }}}}}

showListDyn :: String -> (EvalMonad Dynamic, EvalMonad Dynamic) -> EvalMonad String
showListDyn sep (hd,tl) = do
    hd' <- hd
    tl' <- tl
    case (fromDynamic tl' :: Maybe ()) of {
       Just _  -> showDyn hd';
       Nothing ->
    case (fromDynamic tl' :: Maybe (EvalMonad Dynamic,EvalMonad Dynamic)) of {
       Just p  -> (do hs <- showDyn hd';ts <- showListDyn sep p;return $ hs++sep++ts);
       Nothing -> throwError "type error"
    }}

{-
showTerm :: Fix Term -> [Char]
showTerm (In f) = showIn f
    where showIn (ArithT t) = showArithTerm t
          showIn (LambdaT t) = showLambdaTerm t
          showIn (RelT t) = showRelTerm t
          showIn (ListT t) = showListTerm t
          showArithTerm (Num n) = "(Num "++show n++")"
          showArithTerm (Neg n) = "(Neg "++showTerm n++")"
          showArithTerm (Add l r) = "(Add "++showTerm l++" "++showTerm r++")"
          showArithTerm (Sub l r) = "(Sub "++showTerm l++" "++showTerm r++")"
          showArithTerm (Mul l r) = "(Mul "++showTerm l++" "++showTerm r++")"
          showArithTerm (Div l r) = "(Div "++showTerm l++" "++showTerm r++")"
          showLambdaTerm (Var v) = "(Var "++v++")"
          showLambdaTerm (Lam v b) = "(Lam "++v++" "++showTerm b++")"
          showLambdaTerm (App f' x) = "(App "++showTerm f'++" "++showTerm x++")"
          showRelTerm (Not x) = "(Not "++showTerm x++")"
          showRelTerm (Or l r) = "(Or "++showTerm l++" "++showTerm r++")"
          showRelTerm (And l r) = "(And "++showTerm l++" "++showTerm r++")"
          showRelTerm (Boolean b) = "(Boolean "++show b++")"
          showRelTerm (IfE c t e) = "(If "++showTerm c++" "++showTerm t++" "++showTerm e++")"
          showRelTerm (Equal l r) = "(Equal "++showTerm l++" "++showTerm r++")"
          showRelTerm (NotEqual l r) = "(NotEqual "++showTerm l++" "++showTerm r++")"
          showRelTerm (LessThan l r) = "(LessThan "++showTerm l++" "++showTerm r++")"
          showRelTerm (GreaterThan l r) = "(GreaterThan "++showTerm l++" "++showTerm r++")"
          showRelTerm (LessThanOrEqual l r) = "(LessThanOrEqual "++showTerm l++" "++showTerm r++")"
          showRelTerm (GreaterThanOrEqual l r) = "(GreaterThanOrEqual "++showTerm l++" "++showTerm r++")"
          showListTerm Nil = "Nil"
          showListTerm (Character c) = "(Character '"++c:"')"
          showListTerm (Head l) = "(Head "++showTerm l++")"
          showListTerm (Tail l) = "(Tail "++showTerm l++")"
          showListTerm (Null l) = "(Null "++showTerm l++")"
          showListTerm (Cons l r) = "(Cons "++showTerm l++" "++showTerm r++")"
          showListTerm (Append l r) = "(Append "++showTerm l++" "++showTerm r++")"
-}

instance Pause EvalMonad Result where
    pause = EM . lift . lift . lift . Cont

-- TODO:
-- I need to find a better way to break the circular dependency between
-- Environment and EvalMonad
-- Comment out what isn't true, uncomment what is

{-# NOINLINE evalMonadTypeCon #-}

evalMonadTypeCon :: TyCon
evalMonadTypeCon = mkTyCon "EM"

instance (Typeable a) => Typeable (EvalMonad a) where
#if __GLASGOW_HASKELL__ >= 603
    typeOf _ = mkTyConApp evalMonadTypeCon [typeOf (undefined :: a)]
#else
    typeOf _ = mkAppTy evalMonadTypeCon [typeOf (undefined :: a)]
#endif

instance Monad EvalMonad where
    return = EM . return
    (EM m) >>= f = EM $ m >>= (runEM . f)

-- TODO: make an appropriate instance
instance MonadCont EvalMonad where
       callCC = error "EvalModule/LMEngine: no default instance for callCC"

instance MonadError String EvalMonad where
    catchError (EM m) f = EM $ catchError m (runEM . f)
    throwError = EM . throwError

instance MonadState (IState EvalMonad) EvalMonad where
    put = EM . put
    get = EM get

instance MonadReader Environment EvalMonad where
    local f (EM m) = EM $ local f m
    ask = EM ask

{- this probably won't work without editting
instance MonadWriter EvalMonad where
    pass = EM . pass
    listen = EM . listen
    tell = EM . tell
-}
