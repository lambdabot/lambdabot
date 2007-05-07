module Help where
verboseHelp :: String
verboseHelp = "\
\\n\
\\n\
\Djinn commands explained\n\
\========================\n\
\\n\
\<sym> ? <type>\n\
\  Try to find a function of the specified type.  Djinn knows about the\n\
\function type, tuples, Either, Maybe, (), and can be given new type\n\
\definitions.  (Djinn also knows about the empty type, Void, but this\n\
\is less useful.)  Further functions, type synonyms, and data types can\n\
\be added by using the commands below.  If a function can be found it\n\
\is printed in a style suitable for inclusion in a Haskell program.  If\n\
\no function can be found this will be reported as well.  Examples:\n\
\  Djinn> f ? a->a\n\
\  f :: a -> a\n\
\  f x1 = x1\n\
\  Djinn> sel ? ((a,b),(c,d)) -> (b,c)\n\
\  sel :: ((a, b), (c, d)) -> (b, c)\n\
\  sel ((_, v5), (v6, _)) = (v5, v6)\n\
\  Djinn> cast ? a->b\n\
\  -- cast cannot be realized.\n\
\  Djinn will always find a (total) function if one exists.  (The worst\n\
\case complexity is bad, but unlikely for typical examples.)  If no\n\
\function exists Djinn will always terminate and say so.\n\
\  When multiple implementations of the type exists Djinn will only\n\
\give one of them.  Example:\n\
\  Djinn> f ? a->a->a\n\
\  f :: a -> a -> a\n\
\  f _ x2 = x2\n\
\\n\
\Warning: The given type expression is not checked in any way (i.e., no\n\
\kind checking).\n\
\\n\
\\n\
\<sym> :: <type>\n\
\  Add a new function available for Djinn to construct the result.\n\
\Example:\n\
\  Djinn> foo :: Int -> Char\n\
\  Djinn> bar :: Char -> Bool\n\
\  Djinn> f ? Int -> Bool\n\
\  f :: Int -> Bool\n\
\  f x3 = bar (foo x3)\n\
\This feature is not as powerful as it first might seem.  Djinn does\n\
\*not* instantiate polymorphic function.  It will only use the function\n\
\with exactly the given type.  Example:\n\
\  Djinn> cast :: a -> b\n\
\  Djinn> f ? c->d\n\
\  -- f cannot be realized.\n\
\\n\
\type <sym> <vars> = <type>\n\
\  Add a Haskell style type synonym.  Type synonyms are expanded before\n\
\Djinn starts looking for a realization.\n\
\  Example:\n\
\  Djinn> type Id a = a->a\n\
\  Djinn> f ? Id a\n\
\  f :: Id a\n\
\  f x1 = x1\n\
\\n\
\data <sym> <vars> = <type>\n\
\  Add a Haskell style data type.\n\
\  Example:\n\
\  Djinn> data Foo a = C a a a\n\
\  Djinn> f ? a -> Foo a\n\
\  f :: a -> Foo a\n\
\  f x1 = C x1 x1 x1\n\
\\n\
\\n\
\:clear\n\
\  Set the environment to the start environment.\n\
\\n\
\\n\
\:delete <sym>\n\
\  Remove a symbol that has been added with the add command.\n\
\\n\
\\n\
\:environment\n\
\  List all added symbols and their types.\n\
\\n\
\\n\
\:help\n\
\  Show a short help message.\n\
\\n\
\\n\
\:load <file>\n\
\  Read and execute a file with commands.  The file may include Haskell\n\
\style -- comments.\n\
\\n\
\\n\
\:quit\n\
\  Quit Djinn.\n\
\\n\
\\n\
\:set\n\
\  Set runtime options.\n\
\     +multi    show multiple solutions\n\
\               This will not show all solutions since there might be\n\
\               infinitly many.\n\
\     -multi    show one solution\n\
\     +sorted   sort solutions according to a heuristic criterion\n\
\     -sorted   do not sort solutions\n\
\  The heuristic used to sort the solutions is that as many of the\n\
\bound variables as possible should be used.\n\
\\n\
\:verbose-help\n\
\  Print this message.\n\
\\n\
\\n\
\Further examples\n\
\================\n\
\  calvin% djinn\n\
\  Welcome to Djinn version 2005-12-11.\n\
\  Type :h to get help.\n\
\\n\
\  -- return, bind, and callCC in the continuation monad\n\
\  Djinn> data CD r a = CD ((a -> r) -> r)\n\
\  Djinn> returnCD ? a -> CD r a\n\
\  returnCD :: a -> CD r a\n\
\  returnCD x1 = CD (\\ c15 -> c15 x1)\n\
\\n\
\  Djinn> bindCD ? CD r a -> (a -> CD r b) -> CD r b\n\
\  bindCD :: CD r a -> (a -> CD r b) -> CD r b\n\
\  bindCD x1 x4 =\n\
\           case x1 of\n\
\           CD v3 -> CD (\\ c49 ->\n\
\                        v3 (\\ c50 ->\n\
\                            case x4 c50 of\n\
\                            CD c52 -> c52 c49))\n\
\\n\
\  Djinn> callCCD ? ((a -> CD r b) -> CD r a) -> CD r a\n\
\  callCCD :: ((a -> CD r b) -> CD r a) -> CD r a\n\
\  callCCD x1 =\n\
\            CD (\\ c68 ->\n\
\                case x1 (\\ c69 -> CD (\\ _ -> c68 c69)) of\n\
\                CD c72 -> c72 c68)\n\
\\n\
\\n\
\  -- return and bind in the state monad\n\
\  Djinn> type S s a = (s -> (a, s))\n\
\  Djinn> returnS ? a -> S s a\n\
\  returnS :: a -> S s a\n\
\  returnS x1 x2 = (x1, x2)\n\
\  Djinn> bindS ? S s a -> (a -> S s b) -> S s b\n\
\  bindS :: S s a -> (a -> S s b) -> S s b\n\
\  bindS x1 x2 x3 =\n\
\          case x1 x3 of\n\
\          (v4, v5) -> x2 v4 v5\n\
\\n\
\\n\
\Theory\n\
\======\n\
\  Djinn interprets a Haskell type as a logic formula using the\n\
\Curry-Howard isomorphism and then uses a decision procedure for\n\
\Intuitionistic Propositional Calculus.  This decision procedure is\n\
\based on Gentzen's LJ sequent calculus, but in a modified form, LJT,\n\
\that ensures termination.  This variation on LJ has a long history,\n\
\but the particular formulation used in Djinn is due to Roy Dyckhoff.\n\
\The decision procedure has been extended to generate a proof object\n\
\(i.e., a lambda term).  It is this lambda term (in normal form) that\n\
\constitutes the Haskell code.\n\
\  See http://www.dcs.st-and.ac.uk/~rd/publications/jsl57.pdf for more\n\
\on the exact method used by Djinn.\n\
\\n\
\  Since Djinn handles propositional calculus it also knows about the\n\
\absurd proposition, corresponding to the empty set.  This set is\n\
\called Void in Haskell, and Djinn assumes an elimination rule for the\n\
\Void type:\n\
\  void :: Void -> a\n\
\Using Void is of little use for programming, but can be interesting\n\
\for theorem proving.  Example, the double negation of the law of\n\
\excluded middle:\n\
\  Djinn> f ? Not (Not (Either x (Not x)))\n\
\  f :: Not (Not (Either x (Not x)))\n\
\  f x1 = void (x1 (Right (\\ c23 -> void (x1 (Left c23)))))\n\
\"
