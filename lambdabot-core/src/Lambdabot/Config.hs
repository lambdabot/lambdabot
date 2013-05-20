{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Extensible configuration system for lambdabot
-- 
-- TODO: there's notthing lambdabot-specific about this, it could be a useful standalone library.
module Lambdabot.Config
    ( Config
    , getConfigDefault
    , mergeConfig
    
    , MonadConfig(..)
    
    , config
    , configWithMerge
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Char
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.Maybe
import Data.Typeable
import Language.Haskell.TH

data Config t where Config :: (Typeable1 k, GCompare k) => !(k t) -> t -> (t -> t -> t) -> Config t

cast1 :: (Typeable1 f, Typeable1 g) => f a -> Maybe (g a)
cast1 = fmap runIdentity . gcast1 . Identity

instance GEq Config where
    geq (Config k1 _ _) (Config k2 _ _) = do
        k2' <- cast1 k2
        geq k1 k2'

instance GCompare Config where
    gcompare (Config k1 _ _) (Config k2 _ _) = 
        case compare t1 t2 of
            LT -> GLT
            EQ -> fromMaybe typeErr $ do
                k2'  <- cast1 k2
                return (gcompare k1 k2')
            GT -> GGT
        where
            t1 = typeOf1 k1
            t2 = typeOf1 k2
            
            typeErr = error "TypeReps claim to be equal but cast failed"

getConfigDefault :: Config t -> t
getConfigDefault (Config _ def _) = def

mergeConfig :: Config t -> t -> t -> t
mergeConfig (Config _ _ f) = f

class Monad m => MonadConfig m where
    getConfig :: Config a -> m a

instance  MonadConfig m            => MonadConfig (ReaderT r m) where getConfig = lift . getConfig
instance (MonadConfig m, Monoid w) => MonadConfig (WriterT w m) where getConfig = lift . getConfig
instance  MonadConfig m            => MonadConfig (StateT  s m) where getConfig = lift . getConfig

-- |Define a new configuration key with the specified name, type and 
-- default value
-- 
-- You should probably also provide an explicit export list for any
-- module that defines config keys, because the definition introduces
-- a few extra types that will clutter up the export list otherwise.
config :: String -> TypeQ -> ExpQ -> Q [Dec]
config = configWithMerge [| flip const |]

-- |Like 'config', but also allowing you to specify a \"merge rule\"
-- that will be used to combine multiple bindings of the same key.
-- 
-- For example, in "Lambdabot.Config.Core", 'onStartupCmds' is
-- defined as a list of commands to execute on startup.  Its default
-- value is ["offlinerc"], so if a user invokes the default lambdabot
-- executable without arguments, they will get a REPL.  Each instance
-- of "-e" on the command-line adds a binding of the form:
--
-- > onStartupCmds :=> [command]
-- 
-- So if they give one "-e", it replaces the default (note that it
-- is _not_ merged with the default - the default is discarded), and
-- if they give more than one they are merged using the specified
-- operation (in this case, `(++)`).
configWithMerge :: ExpQ -> String -> TypeQ -> ExpQ -> Q [Dec]
configWithMerge mergeQ nameStr tyQ defValQ = do
    let keyName = mkName nameStr
    tyName      <- newName (map toUpper nameStr)
    conName     <- newName (map toUpper nameStr)
    tyVarName   <- newName "a'"
    
    ty          <- tyQ
    defVal      <- defValQ
    mergeExpr   <- mergeQ
    let tyDec   = DataD [] tyName [PlainTV tyVarName] [ForallC [] [EqualP (VarT tyVarName) ty] (NormalC conName [])] [''Typeable]
        keyDecs =
            [ SigD keyName (AppT (ConT ''Config) ty)
            , ValD (VarP keyName) (NormalB (ConE 'Config `AppE` ConE conName `AppE` defVal `AppE` mergeExpr)) []
            ]
    
    concat <$> sequence
        [ return [tyDec]
        , return keyDecs
        , deriveGEq tyDec
        , deriveGCompare tyDec
        ]
