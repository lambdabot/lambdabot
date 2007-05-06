module Lib.FixPrecedence (withPrec, precTable) where

import qualified Data.Map as M
import Language.Haskell.Syntax
-- import Language.Haskell.Parser  import if uncommenting test code

{-
    PrecedenceData

    This is a data type to hold precedence information.  It simply records,
    for each operator, its precedence level (a number), and associativity
    (one of HsAssocNone, HsAssocLeft, or HsAssocRight).
-}
type PrecedenceData = M.Map HsQOp (HsAssoc, Int)

{-
    findPrec

    Looks up precedence information for a goven operator.  If the operator
    is not in the precedence data, the Haskell report specifies that it
    should be treated as infixl 9.
-}
findPrec :: PrecedenceData -> HsQOp -> (HsAssoc, Int)
findPrec = flip (M.findWithDefault defaultPrec)
    where defaultPrec = (HsAssocLeft, 9)

{-
    <. and >. operators

    Precedence relations between operators are defined here in terms of
    the classic dotted relational operators <. and >.

    For these purposes, only the associativity of the left-most operator
    is considered.  If the associativity of the right-most operator differs,
    this is an error.
-}
precWrong :: PrecedenceData -> HsQOp -> HsQOp -> Bool
precWrong pd a b = let (assoc, prec) = findPrec pd a
                       (_, prec')    = findPrec pd b
                   in     (prec < prec')
                       || (prec == prec' && assoc == HsAssocRight)

{-
    withPrec

    This is the heart of the whole thing.  It applies an algorithm
    described in LaLonde and Rivieres to convert the operators into
    an expression tree according to their correct precedences.
-}
withPrec :: PrecedenceData -> HsExp -> HsExp

withPrec pd (HsInfixApp k@(HsInfixApp e op' f) op g) =
    let g' = withPrec pd g
    in  if precWrong pd op' op
        then withPrec pd (HsInfixApp
                             (withPrec pd e)
                             op'
                             (HsInfixApp (withPrec pd f) op g'))
        else HsInfixApp (withPrec pd k) op g'

withPrec pd (HsInfixApp e op f) = HsInfixApp (withPrec pd e) op (withPrec pd f)

withPrec _  (HsVar v)                = HsVar v
withPrec _  (HsCon c)                = HsCon c
withPrec _  (HsLit l)                = HsLit l
withPrec pd (HsApp e f)              = HsApp (withPrec pd e) (withPrec pd f)
withPrec pd (HsNegApp e)             = HsNegApp (withPrec pd e)
withPrec pd (HsLambda loc pats e)    = let pats' = map (withPrecPat pd) pats
                                       in  HsLambda loc pats' (withPrec pd e)
withPrec pd (HsLet decls e)          = let decls' = map (withPrecDecl pd) decls
                                       in  HsLet decls' (withPrec pd e)
withPrec pd (HsIf e f g)             = HsIf (withPrec pd e)
                                            (withPrec pd f)
                                            (withPrec pd g)
withPrec pd (HsCase e alts)          = let alts' = map (withPrecAlt pd) alts
                                       in  HsCase (withPrec pd e) alts'
withPrec pd (HsDo stmts)             = let stmts' = map (withPrecStmt pd) stmts
                                       in  HsDo stmts'
withPrec pd (HsTuple exps)           = let exps' = map (withPrec pd) exps
                                       in  HsTuple exps'
withPrec pd (HsList exps)            = let exps' = map (withPrec pd) exps
                                       in  HsList exps'
withPrec pd (HsParen e)              = HsParen (withPrec pd e)
withPrec pd (HsLeftSection e op)     = HsLeftSection (withPrec pd e) op
withPrec pd (HsRightSection op e)    = HsRightSection op (withPrec pd e)
withPrec pd (HsRecConstr n upd)      = let upd' = map (withPrecUpd pd) upd
                                       in  HsRecConstr n upd'
withPrec pd (HsRecUpdate e upd)      = let upd' = map (withPrecUpd pd) upd
                                       in  HsRecUpdate (withPrec pd e) upd'
withPrec pd (HsEnumFrom e)           = HsEnumFrom (withPrec pd e)
withPrec pd (HsEnumFromTo e f)       = HsEnumFromTo (withPrec pd e)
                                                    (withPrec pd f)
withPrec pd (HsEnumFromThenTo e f g) = HsEnumFromThenTo (withPrec pd e)
                                                        (withPrec pd f)
                                                        (withPrec pd g)
withPrec pd (HsListComp e stmts)     = let stmts' = map (withPrecStmt pd) stmts
                                       in  HsListComp (withPrec pd e) stmts'
withPrec pd (HsExpTypeSig l e t)     = HsExpTypeSig l (withPrec pd e) t
withPrec pd (HsAsPat n e)            = HsAsPat n (withPrec pd e)
withPrec _  (HsWildCard)             = HsWildCard
withPrec pd (HsIrrPat e)             = HsIrrPat (withPrec pd e)

withPrecPat :: PrecedenceData -> HsPat -> HsPat

-- XXX: not sure what to do here
withPrecPat _  (HsPInfixApp _ _  _) = undefined
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

withPrecPatField :: PrecedenceData -> HsPatField -> HsPatField
withPrecPatField pd (HsPFieldPat n p) = HsPFieldPat n (withPrecPat pd p)

-- XXX: This needs to be completed, or let bindings won't get fixed
withPrecDecl :: PrecedenceData -> HsDecl -> HsDecl
withPrecDecl _  d = d

withPrecAlt :: PrecedenceData -> HsAlt -> HsAlt
withPrecAlt pd (HsAlt l p alts ds) = let ds' = map (withPrecDecl pd) ds
                                     in HsAlt l
                                              (withPrecPat pd p)
                                              (withPrecGAlts pd alts)
                                              ds'

withPrecGAlts :: PrecedenceData -> HsGuardedAlts -> HsGuardedAlts
withPrecGAlts pd (HsUnGuardedAlt e) = HsUnGuardedAlt (withPrec pd e)
withPrecGAlts pd (HsGuardedAlts alts) = let alts' = map (withPrecGAlt pd) alts
                                        in  HsGuardedAlts alts'

withPrecGAlt :: PrecedenceData -> HsGuardedAlt -> HsGuardedAlt
withPrecGAlt pd (HsGuardedAlt l e f) = HsGuardedAlt l
                                                    (withPrec pd e)
                                                    (withPrec pd f)

withPrecStmt :: PrecedenceData -> HsStmt -> HsStmt
withPrecStmt pd (HsGenerator l p e) = HsGenerator l
                                                  (withPrecPat pd p)
                                                  (withPrec pd e)
withPrecStmt pd (HsQualifier e) = HsQualifier (withPrec pd e)
withPrecStmt pd (HsLetStmt ds)  = let ds' = map (withPrecDecl pd) ds
                                  in  HsLetStmt ds'

withPrecUpd :: PrecedenceData -> HsFieldUpdate -> HsFieldUpdate
withPrecUpd pd (HsFieldUpdate n e) = HsFieldUpdate n (withPrec pd e)

precTable :: PrecedenceData
precTable = M.fromList
    [
        (HsQVarOp (UnQual (HsSymbol "!!")),      (HsAssocLeft,  9)),
        (HsQVarOp (UnQual (HsSymbol ".")),       (HsAssocRight, 9)),
        (HsQVarOp (UnQual (HsSymbol "^")),       (HsAssocRight, 8)),
        (HsQVarOp (UnQual (HsSymbol "^^")),      (HsAssocRight, 8)),
        (HsQVarOp (UnQual (HsSymbol "**")),      (HsAssocLeft,  8)),
        (HsQVarOp (UnQual (HsSymbol "*")),       (HsAssocLeft,  7)),
        (HsQVarOp (UnQual (HsSymbol "/")),       (HsAssocLeft,  7)),
        (HsQVarOp (UnQual (HsIdent  "div")),     (HsAssocLeft,  7)),
        (HsQVarOp (UnQual (HsIdent  "mod")),     (HsAssocLeft,  7)),
        (HsQVarOp (UnQual (HsIdent  "rem")),     (HsAssocLeft,  7)),
        (HsQVarOp (UnQual (HsIdent  "quot")),    (HsAssocLeft,  7)),
        (HsQVarOp (UnQual (HsSymbol "+")),       (HsAssocLeft,  6)),
        (HsQVarOp (UnQual (HsSymbol "-")),       (HsAssocLeft,  6)),
        (HsQVarOp (UnQual (HsSymbol ":")),       (HsAssocRight, 5)),
        (HsQVarOp (UnQual (HsSymbol "++")),      (HsAssocRight, 5)),
        (HsQVarOp (UnQual (HsSymbol "==")),      (HsAssocNone,  4)),
        (HsQVarOp (UnQual (HsSymbol "/=")),      (HsAssocNone,  4)),
        (HsQVarOp (UnQual (HsSymbol "<")),       (HsAssocNone,  4)),
        (HsQVarOp (UnQual (HsSymbol "<=")),      (HsAssocNone,  4)),
        (HsQVarOp (UnQual (HsSymbol ">")),       (HsAssocNone,  4)),
        (HsQVarOp (UnQual (HsSymbol ">=")),      (HsAssocNone,  4)),
        (HsQVarOp (UnQual (HsIdent  "elem")),    (HsAssocNone,  4)),
        (HsQVarOp (UnQual (HsIdent  "notElem")), (HsAssocNone,  4)),
        (HsQVarOp (UnQual (HsSymbol "&&")),      (HsAssocRight, 3)),
        (HsQVarOp (UnQual (HsSymbol "||")),      (HsAssocRight, 2)),
        (HsQVarOp (UnQual (HsSymbol ">>")),      (HsAssocLeft,  1)),
        (HsQVarOp (UnQual (HsSymbol ">>=")),     (HsAssocLeft,  1)),
        (HsQVarOp (UnQual (HsSymbol "$")),       (HsAssocRight, 0)),
        (HsQVarOp (UnQual (HsSymbol "$!")),      (HsAssocRight, 0)),
        (HsQVarOp (UnQual (HsIdent  "seq")),     (HsAssocRight, 0))
    ]

{-
    Largely stolen from Pointful, used for testing

modifyOk :: (t -> a) -> ParseResult t -> ParseResult a
modifyOk f r = case r of
  ParseOk v -> ParseOk (f v)
  ParseFailed l m -> ParseFailed l m

parseExpr :: String -> ParseResult HsExp
parseExpr s = modifyOk (\(HsModule _ _ _ _ [HsPatBind _ _ (HsUnGuardedRhs e) []]) -> e)
                       (parseModule ("main = " ++ s))

test :: IO ()
test = let ParseOk exp1 = parseExpr "1 + 2 * 4"
           ParseOk exp2 = parseExpr "if 3 * 4 + 2 == 5 then 3 else 2 + 5^2"
       in  do print $ withPrec precTable exp1
              print $ withPrec precTable exp2
-}
