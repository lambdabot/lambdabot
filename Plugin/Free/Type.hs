{-# OPTIONS -w #-}

module Plugin.Free.Type where

import Control.Monad
import Plugin.Free.Parse
import Data.List
import Plugin.Free.Util

type TyVar = String
type TyName = String

data Type
    = TyForall TyVar Type
    | TyArr Type Type
    | TyTuple [Type]
    | TyCons TyName [Type]
    | TyVar TyVar
        deriving (Eq, Show)

precTYAPP, precARROW :: Int
precTYAPP = 11
precARROW = 10

instance Pretty Type where
    prettyP p (TyForall v t)
        = prettyParen (p > 0) (
            text "forall" <+> text v <> text "." <+> prettyP 0 t
        )
    prettyP p (TyArr t1 t2)
        = prettyParen (p > precARROW) (
            prettyP (precARROW+1) t1 <+> text "->" <+> prettyP precARROW t2
        )
    prettyP _ (TyTuple [])
        = parens empty
    prettyP _ (TyTuple (t:ts))
        = parens (prettyP 0 t <> prettyTs 0 (text ",") ts)
    prettyP _ (TyCons "[]" [t])
        = lbrack <> prettyP 0 t <> rbrack
    prettyP p (TyCons cons ts)
        = prettyParen (p > precTYAPP) (
            text cons <> prettyTs (precTYAPP+1) empty ts
        )
    prettyP _ (TyVar v)
        = text v

prettyTs :: Int -> Doc -> [Type] -> Doc
prettyTs p c [] = empty
prettyTs p c (t:ts) = c <+> prettyP p t <> prettyTs p c ts


parseType :: ParseS Type
parseType
    = parseType' >>= return . normaliseType

parseType' :: ParseS Type
parseType'
    = do
        t <- peekToken
        case t of
            Just IdForall -> getToken >> parseForall
            _             -> parseArrType
    where
        parseForall
            = do
                t <- getToken
                case t of
                    Just (QVarId v)
                        -> parseForall >>= \t -> return (TyForall v t)
                    Just (QVarSym ".")
                        -> parseType'
                    _   -> fail "Expected variable or '.'"

        parseArrType
            = do
                t1 <- parseBType
                t <- peekToken
                case t of
                    Just OpArrow
                        -> getToken >> parseType' >>= \t2 ->
                            return (TyArr t1 t2)
                    _   -> return t1

        parseBType
            = do
                t1 <- parseAType
                case t1 of
                    TyCons c ts
                        -> do
                            ts' <- parseBTypes
                            return (TyCons c (ts++ts'))
                    _   -> return t1

        parseBTypes
            = (parseBType >>= \t -> parseBTypes >>= \ts -> return (t:ts))
                `mplus` return []

        parseAType
            = parseQTyCon `mplus` parseOtherAType

        parseQTyCon
            = do
                t <- getToken
                case t of
                    Just OpenParen
                        -> do
                            t <- getToken
                            case t of
                                Just CloseParen
                                    -> return (TyCons "()" [])
                                Just OpArrow
                                    -> match CloseParen
                                        >> return (TyCons "->" [])
                                Just Comma
                                    -> parseQTyConTuple 1
                                _   -> fail "Badly formed type constructor"
                    Just OpenBracket
                        -> match CloseBracket >> return (TyCons "[]" [])
                    Just (QConId v)
                        -> return (TyCons v [])
                    _   -> fail "Badly formed type constructor"

        parseQTyConTuple :: Int -> ParseS Type
        parseQTyConTuple i
            = do
                t <- getToken
                case t of
                    Just Comma
                        -> parseQTyConTuple (i+1)
                    Just CloseParen
                        -> return (TyCons ("(" ++ take i (repeat ',') ++ ")") [])
                    _   -> fail "Badly formed type constructor"

        parseOtherAType
            = do
                t1 <- getToken
                case t1 of
                    Just OpenParen
                        -> do
                            t <- parseType'
                            parseTuple [t]
                    Just OpenBracket
                        -> parseType' >>= \t -> match CloseBracket
                                        >> return (TyCons "[]" [t])
                    Just (QVarId v)
                        -> return (TyVar v)
                    _   -> fail "Badly formed type"

        parseTuple ts
            = do
                t1 <- getToken
                case t1 of
                    Just CloseParen
                        -> case ts of
                                [t] -> return t
                                _   -> return (TyTuple (reverse ts))
                    Just Comma
                        -> do
                            t <- parseType'
                            parseTuple (t:ts)

normaliseType :: Type -> Type
normaliseType t
    = let (fvs,nt) = normaliseType' t
      in foldr TyForall nt (nub fvs)
    where
        normaliseType' t@(TyVar v)
            = ([v],t)
        normaliseType' (TyForall v t')
            = let (fvs,t) = normaliseType' t'
              in (filter (/=v) fvs, TyForall v t)
        normaliseType' (TyArr t1 t2)
            = let
                (fvs1,t1') = normaliseType' t1
                (fvs2,t2') = normaliseType' t2
              in
                (fvs1++fvs2, TyArr t1' t2')
        normaliseType' (TyTuple ts)
            = let
                fvsts = map normaliseType' ts
                fvs = concat (map fst fvsts)
                ts' = map snd fvsts
              in (fvs, TyTuple ts')
        normaliseType' (TyCons c ts)
            = let
                fvsts = map normaliseType' ts
                fvs = concat (map fst fvsts)
                ts' = map snd fvsts
              in case c of
                    "->" -> case ts' of
                        [t1,t2] -> (fvs, TyArr t1 t2)
                        _ -> error "Arrow type should have 2 arguments"
                    _ -> case checkTuple c of
                        Just i
                            -> if i == length ts'
                                then (fvs, TyTuple ts')
                                else error "Tuple type has the wrong number of arguments"
                        Nothing
                            -> (fvs, TyCons c ts')

        checkTuple ('(':')':cs)
            = Just 0
        checkTuple ('(':cs)
            = checkTuple' 1 cs
        checkTuple _
            = Nothing

        checkTuple' k ")"
            = Just k
        checkTuple' k (',':cs)
            = checkTuple' (k+1) cs
        checkTuple' _ _
            = Nothing

readType :: String -> Type
readType s
    = case parse parseType (lexer s) of
        ParseSuccess t [] -> t
        ParseSuccess t _  -> error "Extra stuff at end of type"
        ParseError msg    -> error msg

-- vim: ts=4:sts=4:expandtab:ai
