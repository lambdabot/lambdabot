{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
----------------------------------------------------------------------
-- |
-- Module      : Plugin.UnMtl
-- Copyright   : Don Stewart, Lennart Kolmodin 2007, Twan van Laarhoven 2008
-- License     : GPL-style (see LICENSE)
--
-- Unroll the MTL monads with your favorite bot!
--
----------------------------------------------------------------------

module Plugin.UnMtl where

import Control.Monad.Error ()

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Lambdabot.Parser (prettyPrintInLine)

import Plugin as P

$(plugin "UnMtl")

instance P.Module UnMtlModule () where
    moduleCmds   _ = ["unmtl"]
    moduleHelp _ _ = "unroll mtl monads"
    process_ _ _ mtl =
        return $ [ either ("err: "++) prettyPrintInLine (mtlParser mtl) ]

-----------------------------------------------------------
-- 'PType' wrapper type

data PMonad a = PMonad
       { pResult :: a                      -- The result (trsnsformed type)
       , pError  :: Maybe String           -- An error message?
       , pFun    :: Maybe (PType -> PType) -- A type function
       }

type PType = PMonad HsType

-- A monad instance so we get things like liftM and sequence for free
instance Monad PMonad where
    return t = PMonad t Nothing Nothing
    m >>= g  = let x = g (pResult m)
               in PMonad (pResult x) (pError m `mplus` pError x) Nothing

-----------------------------------------------------------
-- Lifiting function types

type P = PType

lift0 :: P                            -> HsType -> P
lift1 :: (P -> P)                     -> HsType -> P
lift2 :: (P -> P -> P)                -> HsType -> P
lift3 :: (P -> P -> P -> P)           -> HsType -> P
lift4 :: (P -> P -> P -> P -> P)      -> HsType -> P
lift5 :: (P -> P -> P -> P -> P -> P) -> HsType -> P

lift0 f _ = f
lift1 f n = mkPfun n (lift0 . f)
lift2 f n = mkPfun n (lift1 . f)
lift3 f n = mkPfun n (lift2 . f)
lift4 f n = mkPfun n (lift3 . f)
lift5 f n = mkPfun n (lift4 . f)

mkPfun :: HsType -> (PType -> HsType -> PType) -> PType
mkPfun n cont = PMonad n (Just msg) (Just fun)
  where fun p = cont p (HsTyApp n (pResult p))
        msg = "`" ++ prettyPrintInLine n ++ "' is not applied to enough arguments" ++ full fun ['A'..'Z'] "/\\"
        full p (x:xs) l = case p (con [x]) of
                   PMonad{pFun    = Just p'} -> full p' xs l'
                   PMonad{pError  = Just _}  -> "."
                   PMonad{pResult = t }      -> ", giving `" ++ init l' ++ ". " ++ prettyPrintInLine t ++ "'"
          where l' = l ++ [x] ++ " "

-----------------------------------------------------------
-- Helpers for constructing types

infixr 5 -->
infixl 6 $$

-- Function type
(-->) :: PType -> PType -> PType
a --> b = liftM2 cu a b

cu :: HsType -> HsType -> HsType
cu (HsTyTuple xs) y = foldr HsTyFun y xs
cu a b = HsTyFun a b

-- Type application:
--   If we have a type function, use that
--   Otherwise use HsTyApp, but check for stupid errors
($$) :: PType -> PType -> PType
($$) PMonad{ pFun=Just f } x = f x
($$) f x = PMonad
         { pResult = HsTyApp (pResult f) (pResult x)
         , pError  = pError f `mplus` -- ignore errors in x, the type constructor f might have a higher kind and ignore x
                      if isFunction (pResult f) then Nothing else
                            Just $ "`" ++ prettyPrintInLine (pResult f) ++ "' is not a type function."
         , pFun    = Nothing
         }
  where
    isFunction (HsTyFun _ _) = False
    isFunction (HsTyTuple _) = False
    isFunction _             = True

con, var :: String -> PType
con = return . HsTyCon . UnQual . HsIdent
var = return . HsTyVar . HsIdent

tuple :: [PType] -> PType
tuple = liftM (HsTyTuple . concatMap unpack) . sequence
    where
    unpack (HsTyTuple xs) = xs
    unpack x = [x]

-- a bit of a hack
forall_ :: String -> (PType -> PType) -> PType
forall_ x f = var ("forall "++x++".") $$ f (var x)

-----------------------------------------------------------
-- Definitions from the MTL library

-- MTL types (plus MaybeT)
types :: [(String, HsType -> PType)]
types =
    [ ("Cont",     lift2 $ \r       a -> (a -->      r) -->      r)
    , ("ContT",    lift3 $ \r     m a -> (a --> m $$ r) --> m $$ r)
    , ("ErrorT",   lift3 $ \e     m a -> m $$ (con "Either" $$ e $$ a))
    , ("Identity", lift1 $ \        a -> a)
    , ("ListT",    lift2 $ \      m a -> m $$ (return list_tycon $$ a))
    , ("RWS",      lift4 $ \r w s   a -> r --> s -->      tuple [a, s, w])
    , ("RWST",     lift5 $ \r w s m a -> r --> s --> m $$ tuple [a, s, w])
    , ("Reader",   lift2 $ \r       a -> r -->            a)
    , ("ReaderT",  lift3 $ \r     m a -> r -->       m $$ a)
    , ("Writer",   lift2 $ \  w     a ->                  tuple [a,    w])
    , ("WriterT",  lift3 $ \  w   m a ->             m $$ tuple [a,    w])
    , ("State",    lift2 $ \    s   a ->       s -->      tuple [a, s   ])
    , ("StateT",   lift3 $ \    s m a ->       s --> m $$ tuple [a, s   ])
    -- very common:
    , ("MaybeT",   lift2 $ \      m a -> m $$ (con "Maybe" $$ a))
    -- from the Haskell wiki
    , ("Rand",     lift2 $ \g       a -> g -->      tuple [a, g])
    , ("RandT",    lift3 $ \g     m a -> g --> m $$ tuple [a, g])
    , ("NonDet",   lift1 $ \        a -> forall_ "b" $ \b -> (a --> b --> b) --> b --> b)
    , ("NonDetT",  lift2 $ \      m a -> forall_ "b" $ \b -> (a --> m $$ b --> m $$ b) --> m $$ b --> m $$ b)
    ]

--------------------------------------------------
-- Parsing of types

mtlParser :: String -> Either String HsType
mtlParser input = do
    HsModule _ _ _ _ decls <- liftE $ parseModule ("type X = "++input++"\n")
    hsType <- case decls of
        (HsTypeDecl _ _ _ hsType:_) -> return hsType
        _ -> fail "No parse?"
    let result = mtlParser' hsType
    case pError result of
        Just e  -> fail e
        Nothing -> return (pResult result)
  where
    liftE (ParseOk a) = return a
    liftE (ParseFailed _src str) = fail str

mtlParser' :: HsType -> PType
mtlParser' t@(HsTyCon (UnQual (HsIdent v))) = case lookup v types of
     Just pt -> pt t
     Nothing -> return t
mtlParser' (HsTyApp a b) = mtlParser' a $$ mtlParser' b
mtlParser' t = return t

-----------------------------------------------------------
-- Examples

ex1 :: String
ex1 = "ContT ByteString (StateT s IO) a"
ex2 :: String
ex2 = "StateT s (ContT ByteString IO) a"
ex3 :: String
ex3 = "ErrorT ByteString (WriterT String (State s)) a"
