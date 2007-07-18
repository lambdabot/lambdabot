> {
> {-# OPTIONS -w #-}
> -----------------------------------------------------------------------------
> -- |
> -- Module      :  Lib.Haskell.Parser
> -- Copyright   :  (c) Simon Marlow, Sven Panne 1997-2000
> -- License     :  BSD-style (see the file libraries/base/LICENSE)
> --
> -- Maintainer  :  libraries@haskell.org
> -- Stability   :  experimental
> -- Portability :  portable
> --
> -- Haskell parser, modified to be provide an expression parser.
> --
> -----------------------------------------------------------------------------
>
> module Lib.Parser (
>               parseExpr,
>               ParseMode(..), defaultParseMode, ParseResult(..)) where
>
> import Language.Haskell.Syntax
> import Language.Haskell.ParseMonad
> import Language.Haskell.Lexer
> import Language.Haskell.ParseUtils
> }

ToDo: Check exactly which names must be qualified with Prelude (commas and friends)
ToDo: Inst (MPCs?)
ToDo: Polish constr a bit
ToDo: Ugly: exp0b is used for lhs, pat, exp0, ...
ToDo: Differentiate between record updates and labeled construction.

-----------------------------------------------------------------------------
Conflicts: 2 shift/reduce

2 for ambiguity in 'case x of y | let z = y in z :: Bool -> b'
        (don't know whether to reduce 'Bool' as a btype or shift the '->'.
         Similarly lambda and if.  The default resolution in favour of the
         shift means that a guard can never end with a type signature.
         In mitigation: it's a rare case and no Haskell implementation
         allows these, because it would require unbounded lookahead.)
        There are 2 conflicts rather than one because contexts are parsed
        as btypes (cf ctype).

-----------------------------------------------------------------------------

> %token
>       VARID    { VarId $$ }
>       QVARID   { QVarId $$ }
>       CONID    { ConId $$ }
>       QCONID   { QConId $$ }
>       VARSYM   { VarSym $$ }
>       CONSYM   { ConSym $$ }
>       QVARSYM  { QVarSym $$ }
>       QCONSYM  { QConSym $$ }
>       INT      { IntTok $$ }
>       RATIONAL { FloatTok $$ }
>       CHAR     { Character $$ }
>       STRING   { StringTok $$ }

Symbols

>       '('     { LeftParen }
>       ')'     { RightParen }
>       ';'     { SemiColon }
>       '{'     { LeftCurly }
>       '}'     { RightCurly }
>       vccurly { VRightCurly }                 -- a virtual close brace
>       '['     { LeftSquare }
>       ']'     { RightSquare }
>       ','     { Comma }
>       '_'     { Underscore }
>       '`'     { BackQuote }

Reserved operators

>       '..'    { DotDot }
>       ':'     { Colon }
>       '::'    { DoubleColon }
>       '='     { Equals }
>       '\\'    { Backslash }
>       '|'     { Bar }
>       '<-'    { LeftArrow }
>       '->'    { RightArrow }
>       '@'     { At }
>       '~'     { Tilde }
>       '=>'    { DoubleArrow }
>       '-'     { Minus }
>       '!'     { Exclamation }

Reserved Ids

>       'as'            { KW_As }
>       'case'          { KW_Case }
>       'class'         { KW_Class }
>       'data'          { KW_Data }
>       'default'       { KW_Default }
>       'deriving'      { KW_Deriving }
>       'do'            { KW_Do }
>       'else'          { KW_Else }
>       'hiding'        { KW_Hiding }
>       'if'            { KW_If }
>       'import'        { KW_Import }
>       'in'            { KW_In }
>       'infix'         { KW_Infix }
>       'infixl'        { KW_InfixL }
>       'infixr'        { KW_InfixR }
>       'instance'      { KW_Instance }
>       'let'           { KW_Let }
>       'module'        { KW_Module }
>       'newtype'       { KW_NewType }
>       'of'            { KW_Of }
>       'then'          { KW_Then }
>       'type'          { KW_Type }
>       'where'         { KW_Where }
>       'qualified'     { KW_Qualified }

> %monad { P }
> %lexer { lexer } { EOF }
> %name parse exp
> %tokentype { Token }
> %%

-----------------------------------------------------------------------------
Module Header

> module :: { HsModule }
>       : srcloc 'module' modid maybeexports 'where' body
>               { HsModule $1 $3 $4 (fst $6) (snd $6) }
>       | srcloc body
>               { HsModule $1 main_mod (Just [HsEVar (UnQual main_name)])
>                                                       (fst $2) (snd $2) }

> body :: { ([HsImportDecl],[HsDecl]) }
>       : '{'  bodyaux '}'                      { $2 }
>       | open bodyaux close                    { $2 }

> bodyaux :: { ([HsImportDecl],[HsDecl]) }
>       : optsemis impdecls semis topdecls      { (reverse $2, $4) }
>       | optsemis                topdecls      { ([], $2) }
>       | optsemis impdecls optsemis            { (reverse $2, []) }
>       | optsemis                              { ([], []) }

> semis :: { () }
>       : optsemis ';'                          { () }

> optsemis :: { () }
>       : semis                                 { () }
>       | {- empty -}                           { () }

-----------------------------------------------------------------------------
The Export List

> maybeexports :: { Maybe [HsExportSpec] }
>       :  exports                              { Just $1 }
>       |  {- empty -}                          { Nothing }

> exports :: { [HsExportSpec] }
>       : '(' exportlist optcomma ')'           { reverse $2 }
>       | '(' optcomma ')'                      { [] }

> optcomma :: { () }
>       : ','                                   { () }
>       | {- empty -}                           { () }

> exportlist :: { [HsExportSpec] }
>       :  exportlist ',' export                { $3 : $1 }
>       |  export                               { [$1]  }

> export :: { HsExportSpec }
>       :  qvar                                 { HsEVar $1 }
>       |  qtyconorcls                          { HsEAbs $1 }
>       |  qtyconorcls '(' '..' ')'             { HsEThingAll $1 }
>       |  qtyconorcls '(' ')'                  { HsEThingWith $1 [] }
>       |  qtyconorcls '(' cnames ')'           { HsEThingWith $1 (reverse $3) }
>       |  'module' modid                       { HsEModuleContents $2 }

-----------------------------------------------------------------------------
Import Declarations

> impdecls :: { [HsImportDecl] }
>       : impdecls semis impdecl                { $3 : $1 }
>       | impdecl                               { [$1] }

> impdecl :: { HsImportDecl }
>       : srcloc 'import' optqualified modid maybeas maybeimpspec
>                               { HsImportDecl $1 $4 $3 $5 $6 }

> optqualified :: { Bool }
>       : 'qualified'                           { True  }
>       | {- empty -}                           { False }

> maybeas :: { Maybe Module }
>       : 'as' modid                            { Just $2 }
>       | {- empty -}                           { Nothing }


> maybeimpspec :: { Maybe (Bool, [HsImportSpec]) }
>       : impspec                               { Just $1 }
>       | {- empty -}                           { Nothing }

> impspec :: { (Bool, [HsImportSpec]) }
>       : opthiding '(' importlist optcomma ')' { ($1, reverse $3) }
>       | opthiding '(' optcomma ')'            { ($1, []) }

> opthiding :: { Bool }
>       : 'hiding'                              { True }
>       | {- empty -}                           { False }

> importlist :: { [HsImportSpec] }
>       :  importlist ',' importspec            { $3 : $1 }
>       |  importspec                           { [$1]  }

> importspec :: { HsImportSpec }
>       :  var                                  { HsIVar $1 }
>       |  tyconorcls                           { HsIAbs $1 }
>       |  tyconorcls '(' '..' ')'              { HsIThingAll $1 }
>       |  tyconorcls '(' ')'                   { HsIThingWith $1 [] }
>       |  tyconorcls '(' cnames ')'            { HsIThingWith $1 (reverse $3) }

> cnames :: { [HsCName] }
>       :  cnames ',' cname                     { $3 : $1 }
>       |  cname                                { [$1]  }

> cname :: { HsCName }
>       :  var                                  { HsVarName $1 }
>       |  con                                  { HsConName $1 }

-----------------------------------------------------------------------------
Fixity Declarations

> fixdecl :: { HsDecl }
>       : srcloc infix prec ops                 { HsInfixDecl $1 $2 $3 (reverse $4) }

> prec :: { Int }
>       : {- empty -}                           { 9 }
>       | INT                                   {% checkPrec $1 }

> infix :: { HsAssoc }
>       : 'infix'                               { HsAssocNone  }
>       | 'infixl'                              { HsAssocLeft  }
>       | 'infixr'                              { HsAssocRight }

> ops   :: { [HsOp] }
>       : ops ',' op                            { $3 : $1 }
>       | op                                    { [$1] }

-----------------------------------------------------------------------------
Top-Level Declarations

Note: The report allows topdecls to be empty. This would result in another
shift/reduce-conflict, so we don't handle this case here, but in bodyaux.

> topdecls :: { [HsDecl] }
>       : topdecls1 optsemis            {% checkRevDecls $1 }

> topdecls1 :: { [HsDecl] }
>       : topdecls1 semis topdecl       { $3 : $1 }
>       | topdecl                       { [$1] }

> topdecl :: { HsDecl }
>       : srcloc 'type' simpletype '=' type
>                       { HsTypeDecl $1 (fst $3) (snd $3) $5 }
>       | srcloc 'data' ctype '=' constrs deriving
>                       {% do { (cs,c,t) <- checkDataHeader $3;
>                               return (HsDataDecl $1 cs c t (reverse $5) $6) } }
>       | srcloc 'newtype' ctype '=' constr deriving
>                       {% do { (cs,c,t) <- checkDataHeader $3;
>                               return (HsNewTypeDecl $1 cs c t $5 $6) } }
>       | srcloc 'class' ctype optcbody
>                       {% do { (cs,c,vs) <- checkClassHeader $3;
>                               return (HsClassDecl $1 cs c vs $4) } }
>       | srcloc 'instance' ctype optvaldefs
>                       {% do { (cs,c,ts) <- checkInstHeader $3;
>                               return (HsInstDecl $1 cs c ts $4) } }
>       | srcloc 'default' '(' typelist ')'
>                       { HsDefaultDecl $1 $4 }
>       | decl          { $1 }

> typelist :: { [HsType] }
>       : types                         { reverse $1 }
>       | type                          { [$1] }
>       | {- empty -}                   { [] }

> decls :: { [HsDecl] }
>       : optsemis decls1 optsemis      {% checkRevDecls $2 }
>       | optsemis                      { [] }

> decls1 :: { [HsDecl] }
>       : decls1 semis decl             { $3 : $1 }
>       | decl                          { [$1] }

> decl :: { HsDecl }
>       : signdecl                      { $1 }
>       | fixdecl                       { $1 }
>       | valdef                        { $1 }

> decllist :: { [HsDecl] }
>       : '{'  decls '}'                { $2 }
>       | open decls close              { $2 }

> signdecl :: { HsDecl }
>       : srcloc vars '::' ctype        { HsTypeSig $1 (reverse $2) $4 }

ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
instead of qvar, we get another shift/reduce-conflict. Consider the
following programs:

   { (+) :: ... }          only var
   { (+) x y  = ... }      could (incorrectly) be qvar

We re-use expressions for patterns, so a qvar would be allowed in patterns
instead of a var only (which would be correct). But deciding what the + is,
would require more lookahead. So let's check for ourselves...

> vars  :: { [HsName] }
>       : vars ',' var                  { $3 : $1 }
>       | qvar                          {% do { n <- checkUnQual $1;
>                                               return [n] } }

-----------------------------------------------------------------------------
Types

> type :: { HsType }
>       : btype '->' type               { HsTyFun $1 $3 }
>       | btype                         { $1 }

> btype :: { HsType }
>       : btype atype                   { HsTyApp $1 $2 }
>       | atype                         { $1 }

> atype :: { HsType }
>       : gtycon                        { HsTyCon $1 }
>       | tyvar                         { HsTyVar $1 }
>       | '(' types ')'                 { HsTyTuple (reverse $2) }
>       | '[' type ']'                  { HsTyApp list_tycon $2 }
>       | '(' type ')'                  { $2 }

> gtycon :: { HsQName }
>       : qconid                        { $1 }
>       | '(' ')'                       { unit_tycon_name }
>       | '(' '->' ')'                  { fun_tycon_name }
>       | '[' ']'                       { list_tycon_name }
>       | '(' commas ')'                { tuple_tycon_name $2 }


(Slightly edited) Comment from GHC's hsparser.y:
"context => type" vs  "type" is a problem, because you can't distinguish between

        foo :: (Baz a, Baz a)
        bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

with one token of lookahead.  The HACK is to parse the context as a btype
(more specifically as a tuple type), then check that it has the right form
C a, or (C1 a, C2 b, ... Cn z) and convert it into a context.  Blaach!

> ctype :: { HsQualType }
>       : context '=>' type             { HsQualType $1 $3 }
>       | type                          { HsQualType [] $1 }

> context :: { HsContext }
>       : btype                         {% checkContext $1 }

> types :: { [HsType] }
>       : types ',' type                { $3 : $1 }
>       | type  ',' type                { [$3, $1] }

> simpletype :: { (HsName, [HsName]) }
>       : tycon tyvars                  { ($1,reverse $2) }

> tyvars :: { [HsName] }
>       : tyvars tyvar                  { $2 : $1 }
>       | {- empty -}                   { [] }

-----------------------------------------------------------------------------
Datatype declarations

> constrs :: { [HsConDecl] }
>       : constrs '|' constr            { $3 : $1 }
>       | constr                        { [$1] }

> constr :: { HsConDecl }
>       : srcloc scontype               { HsConDecl $1 (fst $2) (snd $2) }
>       | srcloc sbtype conop sbtype    { HsConDecl $1 $3 [$2,$4] }
>       | srcloc con '{' '}'            { HsRecDecl $1 $2 [] }
>       | srcloc con '{' fielddecls '}' { HsRecDecl $1 $2 (reverse $4) }

> scontype :: { (HsName, [HsBangType]) }
>       : btype                         {% do { (c,ts) <- splitTyConApp $1;
>                                               return (c,map HsUnBangedTy ts) } }
>       | scontype1                     { $1 }

> scontype1 :: { (HsName, [HsBangType]) }
>       : btype '!' atype               {% do { (c,ts) <- splitTyConApp $1;
>                                               return (c,map HsUnBangedTy ts++
>                                                       [HsBangedTy $3]) } }
>       | scontype1 satype              { (fst $1, snd $1 ++ [$2] ) }

> satype :: { HsBangType }
>       : atype                         { HsUnBangedTy $1 }
>       | '!' atype                     { HsBangedTy   $2 }

> sbtype :: { HsBangType }
>       : btype                         { HsUnBangedTy $1 }
>       | '!' atype                     { HsBangedTy   $2 }

> fielddecls :: { [([HsName],HsBangType)] }
>       : fielddecls ',' fielddecl      { $3 : $1 }
>       | fielddecl                     { [$1] }

> fielddecl :: { ([HsName],HsBangType) }
>       : vars '::' stype               { (reverse $1, $3) }

> stype :: { HsBangType }
>       : type                          { HsUnBangedTy $1 }     
>       | '!' atype                     { HsBangedTy   $2 }

> deriving :: { [HsQName] }
>       : {- empty -}                   { [] }
>       | 'deriving' qtycls             { [$2] }
>       | 'deriving' '('          ')'   { [] }
>       | 'deriving' '(' dclasses ')'   { reverse $3 }

> dclasses :: { [HsQName] }
>       : dclasses ',' qtycls           { $3 : $1 }
>       | qtycls                        { [$1] }

-----------------------------------------------------------------------------
Class declarations

> optcbody :: { [HsDecl] }
>       : 'where' decllist              {% checkClassBody $2 }
>       | {- empty -}                   { [] }

-----------------------------------------------------------------------------
Instance declarations

> optvaldefs :: { [HsDecl] }
>       : 'where' '{'  valdefs '}'      {% checkClassBody $3 }
>       | 'where' open valdefs close    {% checkClassBody $3 }
>       | {- empty -}                   { [] }

> valdefs :: { [HsDecl] }
>       : optsemis valdefs1 optsemis    {% checkRevDecls $2 }
>       | optsemis                      { [] }

> valdefs1 :: { [HsDecl] }
>       : valdefs1 semis valdef         { $3 : $1 }
>       | valdef                        { [$1] }

-----------------------------------------------------------------------------
Value definitions

> valdef :: { HsDecl }
>       : srcloc exp0b rhs optwhere     {% checkValDef $1 $2 $3 $4 }

> optwhere :: { [HsDecl] }
>       : 'where' decllist              { $2 }
>       | {- empty -}                   { [] }

> rhs   :: { HsRhs }
>       : '=' exp                       {% do { e <- checkExpr $2;
>                                               return (HsUnGuardedRhs e) } }
>       | gdrhs                         { HsGuardedRhss  (reverse $1) }

> gdrhs :: { [HsGuardedRhs] }
>       : gdrhs gdrh                    { $2 : $1 }
>       | gdrh                          { [$1] }

> gdrh :: { HsGuardedRhs }
>       : srcloc '|' exp0 '=' exp       {% do { g <- checkExpr $3;
>                                               e <- checkExpr $5;
>                                               return (HsGuardedRhs $1 g e) } }

-----------------------------------------------------------------------------
Expressions

Note: The Report specifies a meta-rule for lambda, let and if expressions
(the exp's that end with a subordinate exp): they extend as far to
the right as possible.  That means they cannot be followed by a type
signature or infix application.  To implement this without shift/reduce
conflicts, we split exp10 into these expressions (exp10a) and the others
(exp10b).  That also means that only an exp0 ending in an exp10b (an exp0b)
can followed by a type signature or infix application.  So we duplicate
the exp0 productions to distinguish these from the others (exp0a).

> exp   :: { HsExp }
>       : exp0b '::' srcloc ctype       { HsExpTypeSig $3 $1 $4 }
>       | exp0                          { $1 }

> exp0 :: { HsExp }
>       : exp0a                         { $1 }
>       | exp0b                         { $1 }

> exp0a :: { HsExp }
>       : exp0b qop exp10a              { HsInfixApp $1 $2 $3 }
>       | exp10a                        { $1 }

> exp0b :: { HsExp }
>       : exp0b qop exp10b              { HsInfixApp $1 $2 $3 }
>       | exp10b                        { $1 }

> exp10a :: { HsExp }
>       : '\\' srcloc apats '->' exp    { HsLambda $2 (reverse $3) $5 }
>       | 'let' decllist 'in' exp       { HsLet $2 $4 }
>       | 'if' exp 'then' exp 'else' exp { HsIf $2 $4 $6 }

> exp10b :: { HsExp }
>       : 'case' exp 'of' altslist      { HsCase $2 $4 }
>       | '-' fexp                      { HsNegApp $2 }
>       | 'do' stmtlist                 { HsDo $2 }
>       | fexp                          { $1 }

> fexp :: { HsExp }
>       : fexp aexp                     { HsApp $1 $2 }
>       | aexp                          { $1 }

> apats :: { [HsPat] }
>       : apats apat                    { $2 : $1 }
>       | apat                          { [$1] }

> apat :: { HsPat }
>       : aexp                          {% checkPattern $1 }

UGLY: Because patterns and expressions are mixed, aexp has to be split into
two rules: One right-recursive and one left-recursive. Otherwise we get two
reduce/reduce-errors (for as-patterns and irrefutable patters).

Even though the variable in an as-pattern cannot be qualified, we use
qvar here to avoid a shift/reduce conflict, and then check it ourselves
(as for vars above).

> aexp  :: { HsExp }
>       : qvar '@' aexp                 {% do { n <- checkUnQual $1;
>                                               return (HsAsPat n $3) } }
>       | '~' aexp                      { HsIrrPat $2 }
>       | aexp1                         { $1 }

Note: The first two alternatives of aexp1 are not necessarily record
updates: they could be labeled constructions.

> aexp1 :: { HsExp }
>       : aexp1 '{' '}'                 {% mkRecConstrOrUpdate $1 [] }
>       | aexp1 '{' fbinds '}'          {% mkRecConstrOrUpdate $1 (reverse $3) }
>       | aexp2                         { $1 }

According to the Report, the left section (e op) is legal iff (e op x)
parses equivalently to ((e) op x).  Thus e must be an exp0b.

> aexp2 :: { HsExp }
>       : qvar                          { HsVar $1 }
>       | gcon                          { $1 }
>       | literal                       { HsLit $1 }
>       | '(' exp ')'                   { HsParen $2 }
>       | '(' texps ')'                 { HsTuple (reverse $2) }
>       | '[' list ']'                  { $2 }
>       | '(' exp0b qop ')'             { HsLeftSection $2 $3  }
>       | '(' qopm exp0 ')'             { HsRightSection $2 $3 }
>       | '_'                           { HsWildCard }

> commas :: { Int }
>       : commas ','                    { $1 + 1 }
>       | ','                           { 1 }

> texps :: { [HsExp] }
>       : texps ',' exp                 { $3 : $1 }
>       | exp ',' exp                   { [$3,$1] }

-----------------------------------------------------------------------------
List expressions

The rules below are little bit contorted to keep lexps left-recursive while
avoiding another shift/reduce-conflict.

> list :: { HsExp }
>       : exp                           { HsList [$1] }
>       | lexps                         { HsList (reverse $1) }
>       | exp '..'                      { HsEnumFrom $1 }
>       | exp ',' exp '..'              { HsEnumFromThen $1 $3 }
>       | exp '..' exp                  { HsEnumFromTo $1 $3 }
>       | exp ',' exp '..' exp          { HsEnumFromThenTo $1 $3 $5 }
>       | exp '|' quals                 { HsListComp $1 (reverse $3) }

> lexps :: { [HsExp] }
>       : lexps ',' exp                 { $3 : $1 }
>       | exp ',' exp                   { [$3,$1] }

-----------------------------------------------------------------------------
List comprehensions

> quals :: { [HsStmt] }
>       : quals ',' qual                { $3 : $1 }
>       | qual                          { [$1] }

> qual  :: { HsStmt }
>       : pat srcloc '<-' exp           { HsGenerator $2 $1 $4 }
>       | exp                           { HsQualifier $1 }
>       | 'let' decllist                { HsLetStmt $2 }

-----------------------------------------------------------------------------
Case alternatives

> altslist :: { [HsAlt] }
>       : '{'  alts '}'                 { $2 }
>       | open alts close               { $2 }

> alts :: { [HsAlt] }
>       : optsemis alts1 optsemis       { reverse $2 }

> alts1 :: { [HsAlt] }
>       : alts1 semis alt               { $3 : $1 }
>       | alt                           { [$1] }

> alt :: { HsAlt }
>       : srcloc pat ralt optwhere      { HsAlt $1 $2 $3 $4 }

> ralt :: { HsGuardedAlts }
>       : '->' exp                      { HsUnGuardedAlt $2 }
>       | gdpats                        { HsGuardedAlts (reverse $1) }

> gdpats :: { [HsGuardedAlt] }
>       : gdpats gdpat                  { $2 : $1 }
>       | gdpat                         { [$1] }

> gdpat :: { HsGuardedAlt }
>       : srcloc '|' exp0 '->' exp      { HsGuardedAlt $1 $3 $5 }

> pat :: { HsPat }
>       : exp0b                         {% checkPattern $1 }

-----------------------------------------------------------------------------
Statement sequences

As per the Report, but with stmt expanded to simplify building the list
without introducing conflicts.  This also ensures that the last stmt is
an expression.

> stmtlist :: { [HsStmt] }
>       : '{'  stmts '}'                { $2 }
>       | open stmts close              { $2 }

> stmts :: { [HsStmt] }
>       : 'let' decllist ';' stmts      { HsLetStmt $2 : $4 }
>       | pat srcloc '<-' exp ';' stmts { HsGenerator $2 $1 $4 : $6 }
>       | exp ';' stmts                 { HsQualifier $1 : $3 }
>       | ';' stmts                     { $2 }
>       | exp ';'                       { [HsQualifier $1] }
>       | exp                           { [HsQualifier $1] }

-----------------------------------------------------------------------------
Record Field Update/Construction

> fbinds :: { [HsFieldUpdate] }
>       : fbinds ',' fbind              { $3 : $1 }
>       | fbind                         { [$1] }

> fbind :: { HsFieldUpdate }
>       : qvar '=' exp                  { HsFieldUpdate $1 $3 }

-----------------------------------------------------------------------------
Variables, Constructors and Operators.

> gcon :: { HsExp }
>       : '(' ')'               { unit_con }
>       | '[' ']'               { HsList [] }
>       | '(' commas ')'        { tuple_con $2 }
>       | qcon                  { HsCon $1 }

> var   :: { HsName }
>       : varid                 { $1 }
>       | '(' varsym ')'        { $2 }

> qvar  :: { HsQName }
>       : qvarid                { $1 }
>       | '(' qvarsym ')'       { $2 }

> con   :: { HsName }
>       : conid                 { $1 }
>       | '(' consym ')'        { $2 }

> qcon  :: { HsQName }
>       : qconid                { $1 }
>       | '(' gconsym ')'       { $2 }

> varop :: { HsName }
>       : varsym                { $1 }
>       | '`' varid '`'         { $2 }

> qvarop :: { HsQName }
>       : qvarsym               { $1 }
>       | '`' qvarid '`'        { $2 }

> qvaropm :: { HsQName }
>       : qvarsymm              { $1 }
>       | '`' qvarid '`'        { $2 }

> conop :: { HsName }
>       : consym                { $1 }  
>       | '`' conid '`'         { $2 }

> qconop :: { HsQName }
>       : gconsym               { $1 }
>       | '`' qconid '`'        { $2 }

> op    :: { HsOp }
>       : varop                 { HsVarOp $1 }
>       | conop                 { HsConOp $1 }

> qop   :: { HsQOp }
>       : qvarop                { HsQVarOp $1 }
>       | qconop                { HsQConOp $1 }

> qopm  :: { HsQOp }
>       : qvaropm               { HsQVarOp $1 }
>       | qconop                { HsQConOp $1 }

> gconsym :: { HsQName }
>       : ':'                   { list_cons_name }
>       | qconsym               { $1 }

-----------------------------------------------------------------------------
Identifiers and Symbols

> qvarid :: { HsQName }
>       : varid                 { UnQual $1 }
>       | QVARID                { Qual (Module (fst $1)) (HsIdent (snd $1)) }

> varid :: { HsName }
>       : VARID                 { HsIdent $1 }
>       | 'as'                  { as_name }
>       | 'qualified'           { qualified_name }
>       | 'hiding'              { hiding_name }

> qconid :: { HsQName }
>       : conid                 { UnQual $1 }
>       | QCONID                { Qual (Module (fst $1)) (HsIdent (snd $1)) }

> conid :: { HsName }
>       : CONID                 { HsIdent $1 }

> qconsym :: { HsQName }
>       : consym                { UnQual $1 }
>       | QCONSYM               { Qual (Module (fst $1)) (HsSymbol (snd $1)) }

> consym :: { HsName }
>       : CONSYM                { HsSymbol $1 }

> qvarsym :: { HsQName }
>       : varsym                { UnQual $1 }
>       | qvarsym1              { $1 }

> qvarsymm :: { HsQName }
>       : varsymm               { UnQual $1 }
>       | qvarsym1              { $1 }

> varsym :: { HsName }
>       : VARSYM                { HsSymbol $1 }
>       | '-'                   { minus_name }
>       | '!'                   { pling_name }

> varsymm :: { HsName } -- varsym not including '-'
>       : VARSYM                { HsSymbol $1 }
>       | '!'                   { pling_name }

> qvarsym1 :: { HsQName }
>       : QVARSYM               { Qual (Module (fst $1)) (HsSymbol (snd $1)) }

> literal :: { HsLiteral }
>       : INT                   { HsInt $1 }
>       | CHAR                  { HsChar $1 }
>       | RATIONAL              { HsFrac $1 }
>       | STRING                { HsString $1 }

> srcloc :: { SrcLoc }  :       {% getSrcLoc }
 
-----------------------------------------------------------------------------
Layout

> open  :: { () }       :       {% pushCurrentContext }

> close :: { () }
>       : vccurly               { () } -- context popped in lexer.
>       | error                 {% popContext }

-----------------------------------------------------------------------------
Miscellaneous (mostly renamings)

> modid :: { Module }
>       : CONID                 { Module $1 }
>       | QCONID                { Module (fst $1 ++ '.':snd $1) }

> tyconorcls :: { HsName }
>       : conid                 { $1 }

> tycon :: { HsName }
>       : conid                 { $1 }

> qtyconorcls :: { HsQName }
>       : qconid                { $1 }

> qtycls :: { HsQName }
>       : qconid                { $1 }

> tyvar :: { HsName }
>       : varid                 { $1 }

-----------------------------------------------------------------------------

> {
> happyError :: P a
> happyError = fail "Parse error"

> -- | Parse of a string, which should contain a complete Haskell 98 expression
> parseExpr :: String -> ParseResult HsExp
> parseExpr = runParser parse

Compatibility aliases.

> #if __GLASGOW_HASKELL__ >= 606
> as_name = HsIdent "as"
> hiding_name = HsIdent "hiding"
> qualified_name = HsIdent "qualified"
> minus_name = HsSymbol "-"
> pling_name = HsSymbol "!"
> #endif

> }
