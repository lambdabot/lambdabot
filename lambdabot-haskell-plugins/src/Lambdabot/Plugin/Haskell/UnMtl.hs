----------------------------------------------------------------------
-- |
-- Module      : Plugin.UnMtl
-- Copyright   : Don Stewart, Lennart Kolmodin 2007, Twan van Laarhoven 2008
-- License     : GPL-style (see LICENSE)
--
-- Unroll the MTL monads with your favorite bot!
--
----------------------------------------------------------------------

module Lambdabot.Plugin.Haskell.UnMtl (unmtlPlugin) where

import Lambdabot.Plugin
import qualified Lambdabot.Plugin as Lmb (Module)
import Lambdabot.Util.Parser (prettyPrintInLine)

import Control.Applicative
import Control.Monad
import Language.Haskell.Exts as Hs hiding (tuple, var)

unmtlPlugin :: Lmb.Module ()
unmtlPlugin = newModule
    { moduleCmds = return
        [ (command "unmtl")
            { help = say "unroll mtl monads"
            , process = say . either ("err: "++) prettyPrintInLine . mtlParser
            }
        ]
    }

-----------------------------------------------------------
-- 'PType' wrapper type

data PMonad a = PMonad
       { pResult :: a                      -- The result (trsnsformed type)
       , pError  :: Maybe String           -- An error message?
       , pFun    :: Maybe (PType -> PType) -- A type function
       }

type PType = PMonad Type

instance Functor PMonad where
    fmap = liftM

instance Applicative PMonad where
    pure = return
    (<*>) = ap

-- A monad instance so we get things like liftM and sequence for free
instance Monad PMonad where
    return t = PMonad t Nothing Nothing
    m >>= g  = let x = g (pResult m)
               in PMonad (pResult x) (pError m `mplus` pError x) Nothing

-----------------------------------------------------------
-- Lifiting function types

type P = PType

lift0 :: P                            -> Type -> P
lift1 :: (P -> P)                     -> Type -> P
lift2 :: (P -> P -> P)                -> Type -> P
lift3 :: (P -> P -> P -> P)           -> Type -> P
lift4 :: (P -> P -> P -> P -> P)      -> Type -> P
lift5 :: (P -> P -> P -> P -> P -> P) -> Type -> P

lift0 f _ = f
lift1 f n = mkPfun n (lift0 . f)
lift2 f n = mkPfun n (lift1 . f)
lift3 f n = mkPfun n (lift2 . f)
lift4 f n = mkPfun n (lift3 . f)
lift5 f n = mkPfun n (lift4 . f)

mkPfun :: Type -> (PType -> Type -> PType) -> PType
mkPfun n cont = PMonad n (Just msg) (Just fun)
  where fun p = cont p (TyApp n (pResult p))
        msg = "`" ++ prettyPrintInLine n ++ "' is not applied to enough arguments" ++ full fun ['A'..'Z'] "/\\"
        full p (x:xs) l = case p (con [x]) of
                   PMonad{pFun    = Just p'} -> full p' xs l'
                   PMonad{pError  = Just _}  -> "."
                   PMonad{pResult = t }      -> ", giving `" ++ init l' ++ ". " ++ prettyPrintInLine t ++ "'"
          where l' = l ++ [x] ++ " "
        full _ [] _ = error "UnMtl plugin error: ampty list"

-----------------------------------------------------------
-- Helpers for constructing types

infixr 5 -->
infixl 6 $$

-- Function type
(-->) :: PType -> PType -> PType
a --> b = liftM2 cu a b

cu :: Type -> Type -> Type
cu (TyTuple _ xs) y = foldr TyFun y xs
cu a b = TyFun a b

-- Type application:
--   If we have a type function, use that
--   Otherwise use TyApp, but check for stupid errors
($$) :: PType -> PType -> PType
($$) PMonad{ pFun=Just f } x = f x
($$) f x = PMonad
         { pResult = TyApp (pResult f) (pResult x)
         , pError  = pError f `mplus` -- ignore errors in x, the type constructor f might have a higher kind and ignore x
                      if isFunction (pResult f) then Nothing else
                            Just $ "`" ++ prettyPrintInLine (pResult f) ++ "' is not a type function."
         , pFun    = Nothing
         }
  where
    isFunction (TyFun _ _) = False
    isFunction (TyTuple _ _) = False
    isFunction _             = True

con, var :: String -> PType
con = return . TyCon . UnQual . Ident
var = return . TyVar . Ident

tuple :: [PType] -> PType
tuple = liftM (TyTuple Boxed . concatMap unpack) . sequence
    where
    unpack (TyTuple _ xs) = xs
    unpack x = [x]

-- a bit of a hack
forall_ :: String -> (PType -> PType) -> PType
forall_ x f = var ("forall "++x++".") $$ f (var x)

-----------------------------------------------------------
-- Definitions from the MTL library

-- MTL types (plus MaybeT)
types :: [(String, Type -> PType)]
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

mtlParser :: String -> Either String Type
mtlParser input = do
    Hs.Module _ _ _ _ _ _ decls <- liftE $ parseModule ("type X = "++input++"\n")
    hsType <- case decls of
        (TypeDecl _ _ _ hsType:_) -> return hsType
        _ -> fail "No parse?"
    let result = mtlParser' hsType
    case pError result of
        Just e  -> fail e
        Nothing -> return (pResult result)
  where
    liftE (ParseOk a) = return a
    liftE (ParseFailed _src str) = fail str

mtlParser' :: Type -> PType
mtlParser' t@(TyCon (UnQual (Ident v))) = case lookup v types of
     Just pt -> pt t
     Nothing -> return t
mtlParser' (TyApp a b) = mtlParser' a $$ mtlParser' b
mtlParser' (TyParen t) = mtlParser' t
mtlParser' t = return t

-----------------------------------------------------------
-- Examples
--
-- ContT ByteString (StateT s IO) a
-- StateT s (ContT ByteString IO) a
-- ErrorT ByteString (WriterT String (State s)) a
