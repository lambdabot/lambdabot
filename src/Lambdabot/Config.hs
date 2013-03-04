{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Extensible configuration system for lambdabot
-- 
-- TODO: there's notthing lambdabot-specific about this, it could be a useful standalone library.
module Lambdabot.Config
    ( Config
    , getConfigDefault
    
    , MonadConfig(..)
    
    , config
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

data Config t where Config :: (Typeable1 k, GCompare k) => !(k t) -> t -> Config t

cast1 :: (Typeable1 f, Typeable1 g) => f a -> Maybe (g a)
cast1 = fmap runIdentity . gcast1 . Identity

instance GEq Config where
    geq (Config k1 _) (Config k2 _) = do
        k2' <- cast1 k2
        geq k1 k2'

instance GCompare Config where
    gcompare (Config k1 _) (Config k2 _) = 
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
getConfigDefault (Config _ def) = def

class Monad m => MonadConfig m where
    getConfig :: Config a -> m a

instance  MonadConfig m            => MonadConfig (ReaderT r m) where getConfig = lift . getConfig
instance (MonadConfig m, Monoid w) => MonadConfig (WriterT w m) where getConfig = lift . getConfig
instance  MonadConfig m            => MonadConfig (StateT  s m) where getConfig = lift . getConfig

config :: String -> TypeQ -> ExpQ -> Q [Dec]
config nameStr tyQ defValQ = do
    let keyName = mkName nameStr
    tyName      <- newName (map toUpper nameStr)
    conName     <- newName (map toUpper nameStr)
    tyVarName   <- newName "a'"
    
    ty          <- tyQ
    defVal      <- defValQ
    let tyDec   = DataD [] tyName [PlainTV tyVarName] [ForallC [] [EqualP (VarT tyVarName) ty] (NormalC conName [])] [''Typeable]
        keyDecs =
            [ SigD keyName (AppT (ConT ''Config) ty)
            , ValD (VarP keyName) (NormalB (ConE 'Config `AppE` ConE conName `AppE` defVal)) []
            ]
    
    concat <$> sequence
        [ return [tyDec]
        , return keyDecs
        , deriveGEq tyDec
        , deriveGCompare tyDec
        ]
