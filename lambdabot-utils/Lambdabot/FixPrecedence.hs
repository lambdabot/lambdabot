module Lambdabot.FixPrecedence (withPrecExp, withPrecDecl, precTable, FixPrecedence(..) ) where

import qualified Data.Map as M
import Language.Haskell.Syntax
import Data.List

{-
    PrecedenceData

    This is a data type to hold precedence information.  It simply records,
    for each operator, its precedence level (a number), and associativity
    (one of HsAssocNone, HsAssocLeft, or HsAssocRight).
-}
type PrecedenceData = M.Map HsQName (HsAssoc, Int)

{-
    findPrec

    Looks up precedence information for a goven operator.  If the operator
    is not in the precedence data, the Haskell report specifies that it
    should be treated as infixl 9.
-}
findPrec :: PrecedenceData -> HsQName -> (HsAssoc, Int)
findPrec = flip (M.findWithDefault defaultPrec)
    where defaultPrec = (HsAssocLeft, 9)

{-
    precWrong

    This returns True iff the first operator should be a parent of the
    second in the expression tree, when they occur consecutively left to
    right in the input.  This is called "wrong" because the parser in
    Language.Haskell.Parser treats everything as left associative at the
    same precedence, so the right-most operator will be the parent in the
    expression tree in the original input.

    XXX: Currently, this function treats operators with no associativity
         as if they were left associative.  It also looks only at the
         associativity of the left-most operator.  This should work for
         correct code, but it does not report errors for incorrect code.
-}
precWrong :: PrecedenceData -> HsQName -> HsQName -> Bool
precWrong pd a b = let (assoc, prec) = findPrec pd a
                       (_, prec')    = findPrec pd b
                   in     (prec < prec')
                       || (prec == prec' && assoc == HsAssocRight)

{-
    nameFromQOp

    Extracts the HsQName from an HsQOp.
-}
nameFromQOp :: HsQOp -> HsQName
nameFromQOp (HsQVarOp s) = s
nameFromQOp (HsQConOp s) = s

nameFromOp :: HsOp -> HsQName
nameFromOp (HsVarOp n) = UnQual n
nameFromOp (HsConOp n) = UnQual n

{-
    withPrecExp

    This routine fixes up an expression by applying precedence data.
-}
withPrecExp :: PrecedenceData -> HsExp -> HsExp

{-
    This is the heart of the whole thing.  It applies an algorithm
    described by LaLonde and Rivieres in ACM Transactions on Programming
    Languages and Systems, January 1981.  The idea is to take a parse
    tree with a consistent left-associative organization, and rearrange it
    to match a precedence table.

    A few changes have been made.  LaLonde and Rivieres remove parentheses
    from their parse tree, which isn't necessary here; and they work with
    an inherently right-associative grammar, while Language.Haskell.Parser
    produces a left-associative grammar.
-}
withPrecExp pd (HsInfixApp k@(HsInfixApp e qop' f) qop g) =
    let g'  = withPrecExp pd g
        op  = nameFromQOp qop
        op' = nameFromQOp qop'
    in  if precWrong pd op' op
        then let e' = withPrecExp pd e
                 f' = withPrecExp pd f
             in  withPrecExp pd (HsInfixApp e' qop' (HsInfixApp f' qop g'))
        else HsInfixApp (withPrecExp pd k) qop g'

withPrecExp pd (HsInfixApp e op f) =
    HsInfixApp (withPrecExp pd e) op (withPrecExp pd f)

{-
    The remaining cases simply propogate the correction throughout other
    elements of the grammar.
-}
withPrecExp _  (HsVar v)                = HsVar v
withPrecExp _  (HsCon c)                = HsCon c
withPrecExp _  (HsLit l)                = HsLit l
withPrecExp pd (HsApp e f)              =
    HsApp (withPrecExp pd e) (withPrecExp pd f)
withPrecExp pd (HsNegApp e)             =
    HsNegApp (withPrecExp pd e)
withPrecExp pd (HsLambda loc pats e)    =
    let pats' = map (withPrecPat pd) pats
    in  HsLambda loc pats' (withPrecExp pd e)
withPrecExp pd (HsLet decls e)          =
    let (pd', decls') = mapAccumL withPrecDecl pd decls
    in  HsLet decls' (withPrecExp pd' e)
withPrecExp pd (HsIf e f g)             =
    HsIf (withPrecExp pd e) (withPrecExp pd f) (withPrecExp pd g)
withPrecExp pd (HsCase e alts)          =
    let alts' = map (withPrecAlt pd) alts
    in  HsCase (withPrecExp pd e) alts'
withPrecExp pd (HsDo stmts)             =
    let (_, stmts') = mapAccumL withPrecStmt pd stmts
    in  HsDo stmts'
withPrecExp pd (HsTuple exps)           =
    let exps' = map (withPrecExp pd) exps
    in  HsTuple exps'
withPrecExp pd (HsList exps)            =
    let exps' = map (withPrecExp pd) exps
    in  HsList exps'
withPrecExp pd (HsParen e)              =
    HsParen (withPrecExp pd e)
withPrecExp pd (HsLeftSection e op)     =
    HsLeftSection (withPrecExp pd e) op
withPrecExp pd (HsRightSection op e)    =
    HsRightSection op (withPrecExp pd e)
withPrecExp pd (HsRecConstr n upd)      =
    let upd' = map (withPrecUpd pd) upd
    in  HsRecConstr n upd'
withPrecExp pd (HsRecUpdate e upd)      =
    let upd' = map (withPrecUpd pd) upd
    in  HsRecUpdate (withPrecExp pd e) upd'
withPrecExp pd (HsEnumFrom e)           =
    HsEnumFrom (withPrecExp pd e)
withPrecExp pd (HsEnumFromThen e f)     =
    HsEnumFromThen (withPrecExp pd e) (withPrecExp pd f)
withPrecExp pd (HsEnumFromTo e f)       =
    HsEnumFromTo (withPrecExp pd e) (withPrecExp pd f)
withPrecExp pd (HsEnumFromThenTo e f g) =
    HsEnumFromThenTo (withPrecExp pd e) (withPrecExp pd f) (withPrecExp pd g)
withPrecExp pd (HsListComp e stmts)     =
    let (_, stmts') = mapAccumL withPrecStmt pd stmts
    in  HsListComp (withPrecExp pd e) stmts'
withPrecExp pd (HsExpTypeSig l e t)     =
    HsExpTypeSig l (withPrecExp pd e) t
withPrecExp pd (HsAsPat n e)            =
    HsAsPat n (withPrecExp pd e)
withPrecExp _  (HsWildCard)             =
    HsWildCard
withPrecExp pd (HsIrrPat e)             =
    HsIrrPat (withPrecExp pd e)

{-
    This function is analogous to withPrec, but operates on patterns instead
    of expressions.
-}
withPrecPat :: PrecedenceData -> HsPat -> HsPat

{-
    This is the same algorithm based on Lalonde and Rivieres, but designed
    to work with infix data constructors in pattern matching.
-}
withPrecPat pd (HsPInfixApp k@(HsPInfixApp e op' f) op g) =
    let g' = withPrecPat pd g
    in  if precWrong pd op' op
        then let e' = withPrecPat pd e
                 f' = withPrecPat pd f
             in  withPrecPat pd (HsPInfixApp e' op' (HsPInfixApp f' op g'))
        else HsPInfixApp (withPrecPat pd k) op g'

withPrecPat pd (HsPInfixApp e op f) =
    HsPInfixApp (withPrecPat pd e) op (withPrecPat pd f)

withPrecPat _  (HsPVar n)           = HsPVar n
withPrecPat _  (HsPLit l)           = HsPLit l
withPrecPat pd (HsPNeg p)           = HsPNeg (withPrecPat pd p)
withPrecPat pd (HsPApp n ps)        = let ps' = map (withPrecPat pd) ps
                                      in  HsPApp n ps'
withPrecPat pd (HsPTuple ps)        = let ps' = map (withPrecPat pd) ps
                                      in  HsPTuple ps'
withPrecPat pd (HsPList ps)         = let ps' = map (withPrecPat pd) ps
                                      in  HsPList ps'
withPrecPat pd (HsPParen p)         = HsPParen (withPrecPat pd p)
withPrecPat pd (HsPRec n pfs)       = let pfs' = map (withPrecPatField pd) pfs
                                      in  HsPRec n pfs'
withPrecPat pd (HsPAsPat n p)       = HsPAsPat n (withPrecPat pd p)
withPrecPat _  (HsPWildCard)        = HsPWildCard
withPrecPat pd (HsPIrrPat p)        = HsPIrrPat (withPrecPat pd p)

{-
    Propogates precedence fixing through a pattern "field"
-}
withPrecPatField :: PrecedenceData -> HsPatField -> HsPatField
withPrecPatField pd (HsPFieldPat n p) = HsPFieldPat n (withPrecPat pd p)

{-
    Propogates precedence fixing through declaration sections.  This
    gets interesting, because declarations can actually change the
    existing precedence, so withPrecDecl returns both the transformed
    tree and an augmented precedence relation.
-}
withPrecDecl :: PrecedenceData -> HsDecl -> (PrecedenceData, HsDecl)
withPrecDecl pd d@(HsInfixDecl _ assoc p ops)  =
    let nms      = map nameFromOp ops
        prec     = (assoc, p)
        pd'      = M.union pd $ M.fromList $ map (flip (,) prec) nms
    in  (pd', d)
withPrecDecl pd (HsClassDecl l ctx n ns decls) =
    let (pd', decls') = mapAccumL withPrecDecl pd decls
    in  (pd', HsClassDecl l ctx n ns decls')
withPrecDecl pd (HsInstDecl l ctx n ts decls)  =
    -- The question of what to do with fixity declarations here is
    -- interesting.  The report says they aren't allowed (4.3.2), but
    -- GHC accepts them as of version 6.6 and apparently ignores them.
    -- The best thing is probably to match GHC's behavior.
    let decls' = map snd $ map (withPrecDecl pd) decls
    in  (pd, HsInstDecl l ctx n ts decls')
withPrecDecl pd (HsFunBind ms)                 =
    let ms' = map (withPrecMatch pd) ms
    in  (pd, HsFunBind ms')
withPrecDecl pd (HsPatBind l p rhs decls)      =
    let p'     = withPrecPat pd p
        (pd',decls') = mapAccumL withPrecDecl pd decls
        rhs'   = withPrecRhs pd' rhs
    in  (pd, HsPatBind l p' rhs' decls')
withPrecDecl pd d                              = (pd, d)

{-
    Propogates precedence fixing through HsMatch
-}
withPrecMatch :: PrecedenceData -> HsMatch -> HsMatch
withPrecMatch pd (HsMatch l n ps rhs decls)           =
    let ps'           = map (withPrecPat pd) ps
        (pd', decls') = mapAccumL withPrecDecl pd decls
        rhs'          = withPrecRhs pd' rhs
    in  HsMatch l n ps' rhs' decls'

{-
    Propogates precedence fixing through HsRhs
-}
withPrecRhs :: PrecedenceData -> HsRhs -> HsRhs
withPrecRhs pd (HsUnGuardedRhs e)  = HsUnGuardedRhs (withPrecExp pd e)
withPrecRhs pd (HsGuardedRhss grs) = let grs' = map (withPrecGRhs pd) grs
                                     in HsGuardedRhss grs'

withPrecGRhs :: PrecedenceData -> HsGuardedRhs -> HsGuardedRhs
withPrecGRhs pd (HsGuardedRhs l e f) =
    HsGuardedRhs l (withPrecExp pd e) (withPrecExp pd f)

{-
    Propogates precedence fixing through case statement alternatives.
-}
withPrecAlt :: PrecedenceData -> HsAlt -> HsAlt
withPrecAlt pd (HsAlt l p alts ds) =
    let (pd', ds') = mapAccumL withPrecDecl pd ds
    in HsAlt l (withPrecPat pd p) (withPrecGAlts pd' alts) ds'

withPrecGAlts :: PrecedenceData -> HsGuardedAlts -> HsGuardedAlts
withPrecGAlts pd (HsUnGuardedAlt e) = HsUnGuardedAlt (withPrecExp pd e)
withPrecGAlts pd (HsGuardedAlts alts) = let alts' = map (withPrecGAlt pd) alts
                                        in  HsGuardedAlts alts'

withPrecGAlt :: PrecedenceData -> HsGuardedAlt -> HsGuardedAlt
withPrecGAlt pd (HsGuardedAlt l e f) =
    HsGuardedAlt l (withPrecExp pd e) (withPrecExp pd f)

{-
    Propogates precedence fixing through do blocks.  Because let statements
    can change precedence, the result is both the transformed tree and an
    augmented precedence relation, much like in withPrecDecl.
-}
withPrecStmt :: PrecedenceData -> HsStmt -> (PrecedenceData, HsStmt)
withPrecStmt pd (HsGenerator l p e) =
    (pd, HsGenerator l (withPrecPat pd p) (withPrecExp pd e))
withPrecStmt pd (HsQualifier e) = (pd, HsQualifier (withPrecExp pd e))
withPrecStmt pd (HsLetStmt ds)  = let (pd', ds') = mapAccumL withPrecDecl pd ds
                                  in  (pd', HsLetStmt ds')

{-
    Propogates precedence fixing through record field updates.
-}
withPrecUpd :: PrecedenceData -> HsFieldUpdate -> HsFieldUpdate
withPrecUpd pd (HsFieldUpdate n e) = HsFieldUpdate n (withPrecExp pd e)

{-
    This is the default precedence table used for parsing expressions.
    It is taken from the precedences of the main operators in the Haskell
    Prelude.

    XXX: It might be a good idea to search the standard library docs for
         other operators.  These are the ones listed in the Haskell Report
         section 4.  For example, one that is not included here is
         Data.Ratio.%
-}
precTable :: PrecedenceData
precTable = M.fromList
    [
        (UnQual (HsSymbol "!!"),      (HsAssocLeft,  9)),
        (UnQual (HsSymbol "."),       (HsAssocRight, 9)),
        (UnQual (HsSymbol "^"),       (HsAssocRight, 8)),
        (UnQual (HsSymbol "^^"),      (HsAssocRight, 8)),
        (UnQual (HsSymbol "**"),      (HsAssocLeft,  8)),
        (UnQual (HsSymbol "*"),       (HsAssocLeft,  7)),
        (UnQual (HsSymbol "/"),       (HsAssocLeft,  7)),
        (UnQual (HsIdent  "div"),     (HsAssocLeft,  7)),
        (UnQual (HsIdent  "mod"),     (HsAssocLeft,  7)),
        (UnQual (HsIdent  "rem"),     (HsAssocLeft,  7)),
        (UnQual (HsIdent  "quot"),    (HsAssocLeft,  7)),
        (UnQual (HsSymbol "+"),       (HsAssocLeft,  6)),
        (UnQual (HsSymbol "-"),       (HsAssocLeft,  6)),
        (UnQual (HsSymbol ":"),       (HsAssocRight, 5)),
        (Special HsCons,              (HsAssocRight, 5)),
        (UnQual (HsSymbol "++"),      (HsAssocRight, 5)),
        (UnQual (HsSymbol "=="),      (HsAssocNone,  4)),
        (UnQual (HsSymbol "/="),      (HsAssocNone,  4)),
        (UnQual (HsSymbol "<"),       (HsAssocNone,  4)),
        (UnQual (HsSymbol "<="),      (HsAssocNone,  4)),
        (UnQual (HsSymbol ">"),       (HsAssocNone,  4)),
        (UnQual (HsSymbol ">="),      (HsAssocNone,  4)),
        (UnQual (HsIdent  "elem"),    (HsAssocNone,  4)),
        (UnQual (HsIdent  "notElem"), (HsAssocNone,  4)),
        (UnQual (HsSymbol "&&"),      (HsAssocRight, 3)),
        (UnQual (HsSymbol "||"),      (HsAssocRight, 2)),
        (UnQual (HsSymbol ">>"),      (HsAssocLeft,  1)),
        (UnQual (HsSymbol ">>="),     (HsAssocLeft,  1)),
        (UnQual (HsSymbol "$"),       (HsAssocRight, 0)),
        (UnQual (HsSymbol "$!"),      (HsAssocRight, 0)),
        (UnQual (HsIdent  "seq"),     (HsAssocRight, 0))
    ]


class FixPrecedence a where
    fixPrecedence :: a -> a

instance FixPrecedence HsExp where
    fixPrecedence = withPrecExp precTable

instance FixPrecedence HsDecl where
    fixPrecedence = snd . withPrecDecl precTable

