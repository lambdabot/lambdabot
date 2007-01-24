----------------------------------------------------------------------
-- |
-- Module      : Plugin.UnMtl
-- Copyright   : Don Stewart, Lennart Kolmodin 2007
-- License     : GPL-style (see LICENSE)
-- 
-- Unroll the MTL monads with your favorite bot! 
--
----------------------------------------------------------------------

module Plugin.UnMtl where

import Control.Monad.Error ()

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty

import Plugin as P

PLUGIN UnMtl

instance P.Module UnMtlModule () where
    moduleCmds   _ = ["unmtl"]
    moduleHelp _ _ = "unroll mtl monads"
    process_ _ _ mtl =
        return $ [ either ("err: "++) pretty (mtlParser mtl) ]

-----------------------------------------------------------
-- Helpers

infixr 5 -->

(-->) :: HsType -> HsType -> HsType
a --> b = cu a b

cu :: HsType -> HsType -> HsType
cu (HsTyTuple xs) y = foldr HsTyFun y xs
cu a b = HsTyFun a b

app :: HsType -> HsType -> HsType
app = HsTyApp

var :: String -> HsType
var = HsTyVar . HsIdent

con :: String -> HsType
con = HsTyCon . UnQual . HsIdent

tuple :: [HsType] -> HsType
tuple = HsTyTuple

fromCon :: HsType -> Maybe String
fromCon (HsTyCon (UnQual (HsIdent v))) = Just v
fromCon _ = Nothing


-----------------------------------------------------------
-- Definitions from the MTL library

type T = HsType

-- Cont r a = (a  -> r)  -> r
cont :: T -> T -> T
cont    r a = (a --> r) --> r

-- ContT r m a = (a  -> m r)  -> m r
contT :: T -> (T -> T) -> T -> T
contT    r m a = (a --> m r) --> m r

-- Error a = Either s a
error_ :: T -> T
error_   a = app (app (con "Either") (con "String")) a

-- ErrorT e m a = m (Either e a)
errorT :: T -> (T -> T) -> T -> T
errorT    e m a = m (app (app (con "Either") e) a)

-- Identity a = a
identity :: T -> T
identity    a  = a

-- ListT m a = m [a]
listT :: (T -> T) -> T -> T
listT    m a = m (app list_tycon a)

rws :: T -> T -> T -> T -> T
rws  r w s a    = r --> s --> tuple [a, s, w]

rwsT :: T -> T -> T -> (T -> T) -> T -> T
rwsT r w s m a  = r --> s --> m (tuple [a, s, w])

-- Reader r a = r  -> a
reader :: T -> T -> T
reader    r a = r --> a

-- ReaderT r m a = r  -> m a
readerT :: T -> (T -> T) -> T -> T
readerT    r m a = r --> m a

-- State s a = s  -> (a, s)
state :: T -> T -> T
state    s a = s --> (tuple [a, s])

-- StateT s m a = s  -> m (a, s)
stateT :: T -> (T -> T) -> T -> T
stateT    s m a = s --> m (tuple [a, s])

-- Writer w a = (a, w)
writer :: T -> T -> T
writer    w a = tuple [a, w]

-- WriterT w m a = m (a, w)
writerT :: T -> (T -> T) -> T -> T
writerT    w m a = m (tuple [a, w])

--------------------------------------------------
-- Parsing of types

mtlParser :: String -> Either String HsType
mtlParser input = do
    HsModule _ _ _ _ decls <- liftE $ parseModule ("type X = " ++ input)
    hsType <- case decls of
        (HsTypeDecl _ _ _ hsType:_) -> return hsType 
        _ -> fail "No parse?"
    case hsType of
        HsTyApp mtl a -> do
            monad <- mtlParser' mtl
            return (monad a)
        _ -> fail "No applications"
    where
    liftE (ParseOk a) = return a
    liftE (ParseFailed _src str) = fail str

mtlParser' :: HsType -> Either String (HsType -> HsType)
mtlParser' (HsTyApp (HsTyApp (HsTyApp (HsTyApp topMonad r) w) s) m) = do
    mtl' <- mtlParser' m
    mtl <- case fromCon topMonad of
        Just "RWST" -> return rwsT
        _      -> fail "Unknown MTL(4)"
    return (mtl r w s mtl')
mtlParser' (HsTyApp (HsTyApp (HsTyApp topMonad r) w) s) = do
    m <- case fromCon topMonad of
        Just "RWS" -> return rws
        _     -> fail "Unknown MTL(3)"
    return (m r w s)
mtlParser' (HsTyApp (HsTyApp topMonad arg) innerMonad) = do
    mtl' <- mtlParser' innerMonad
    mtl  <- case fromCon topMonad of
        Just "ContT"   -> return contT
        Just "ErrorT"  -> return errorT
        Just "ReaderT" -> return readerT
        Just "StateT"  -> return stateT
        Just "WriterT" -> return writerT
        _         -> fail "Unknown MTL(2)"
    return (mtl arg mtl')
mtlParser' (HsTyApp hsMonad arg) = do
    mtl <- case fromCon hsMonad of
        Just "Cont"   -> return cont
        Just "ListT"  -> do
            mtl' <- mtlParser' arg
            return (const $ listT mtl')
        Just "Reader" -> return reader
        Just "State"  -> return state
        Just "Writer" -> return writer
        _        -> fail "Unknown MTL(1)"
    return (mtl arg)
mtlParser' hsType@(HsTyCon _) =
    case fromCon hsType of
        Just "Error" -> return error_
        _       -> return (HsTyApp hsType)

mtlParser' hsType = return (HsTyApp hsType)

-----------------------------------------------------------
-- Pretty printing

pretty :: HsType -> String
pretty = prettyPrintWithMode (defaultMode { layout = PPNoLayout })

-----------------------------------------------------------
-- Examples

ex1 :: String
ex1 = "ContT ByteString (StateT s IO) a"
ex2 :: String
ex2 = "StateT s (ContT ByteString IO) a"
ex3 :: String
ex3 = "ErrorT ByteString (WriterT String (State s)) a"
