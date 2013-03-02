{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Extensible configuration system for lambdabot
module Lambdabot.Config where

import Control.Applicative
import Control.Monad.Identity
import Data.Char
import Data.Dependent.Sum
import qualified Data.Dependent.Map as D
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.Maybe
import Data.Typeable
import Language.Haskell.TH

-- | The 'Config' type provides configurations for lambdabot. It is used
--   when lambdabot is started to determine the name of lambdabot, what
--   IRC-network lambdabot should join, which channels lambdabot should
--   join upon successful connection, etc.
--
newtype Config = Config (D.DMap ConfigKey)

data ConfigKey t where
    ConfigKey :: (Typeable1 k, GCompare k) => 
        { configKeyTag      :: !(k t)
        , configDefault     :: t
        } -> ConfigKey t

cast1 :: (Typeable1 f, Typeable1 g) => f a -> Maybe (g a)
cast1 = fmap runIdentity . gcast1 . Identity

instance GEq ConfigKey where
    geq (ConfigKey k1 _) (ConfigKey k2 _) = do
        k2' <- cast1 k2
        geq k1 k2'

instance GCompare ConfigKey where
    gcompare (ConfigKey k1 _) (ConfigKey k2 _) = 
        case compare t1 t2 of
            LT -> GLT
            EQ -> fromMaybe typeErr $ do
                k2'  <- cast1 k2
                Refl <- geq k1 k2'
                return (gcompare k1 k2')
            GT -> GGT
        where
            t1 = typeOf1 k1
            t2 = typeOf1 k2
            
            typeErr = error "TypeReps claim to be equal but cast failed"

lookupConfig :: ConfigKey t -> Config -> t
lookupConfig k (Config m) = case D.lookup k m of
    Just x  -> x
    Nothing -> configDefault k

config :: [DSum ConfigKey] -> Config
config = Config . D.fromList

configKey :: String -> TypeQ -> ExpQ -> Q [Dec]
configKey nameStr tyQ defValQ = do
    let keyName = mkName nameStr
    tyName      <- newName (map toUpper nameStr)
    conName     <- newName (map toUpper nameStr)
    tyVarName   <- newName "a'"
    
    ty          <- tyQ
    defVal      <- defValQ
    let tyDec   = DataD [] tyName [PlainTV tyVarName] [ForallC [] [EqualP (VarT tyVarName) ty] (NormalC conName [])] [''Typeable]
        keyDecs =
            [ SigD keyName (AppT (ConT ''ConfigKey) ty)
            , ValD (VarP keyName) (NormalB (ConE 'ConfigKey `AppE` ConE conName `AppE` defVal)) []
            ]
    
    concat <$> sequence
        [ return [tyDec]
        , return keyDecs
        , deriveGEq tyDec
        , deriveGCompare tyDec
        ]
