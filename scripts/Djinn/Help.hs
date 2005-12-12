module Help where
verboseHelp :: String
verboseHelp = "\
\Djinn commands explained\n\
\========================\n\
\\n\
\<sym> ? <type>\n\
\  Try to find a function of the specified type.  Djinn knows about the\n\
\function type, tuples, Either, and the () type.  (Djinn also knows\n\
\about the empty type, Void, but this is less useful.)  Further\n\
\functions and types can be added by using the next command.  If a\n\
\function can be found it is printed in a style suitable for inclusion\n\
\in a Haskell program.  If no function can be found this will be\n\
\reported as well.  Examples:\n\
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
\  Warning: Type synonyms are treated as macros and are not checked in\n\
\any way (e.g., for recursive definitions).\n\
\  Example:\n\
\  Djinn> type Id a = a->a\n\
\  Djinn> f ? Id a\n\
\  f :: Id a\n\
\  f x1 = x1\n\
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
\     +m    show multiple solutions\n\
\     -m    show one solution\n\
\\n\
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
\  Djinn> type C a = (a -> r) -> r\n\
\  Djinn> returnC ? a -> C a\n\
\  returnC :: a -> C a\n\
\  returnC x1 x2 = x2 x1\n\
\\n\
\  Djinn> bindC ? C a -> (a -> C b) -> C b\n\
\  bindC :: C a -> (a -> C b) -> C b\n\
\  bindC x1 x2 x3 = x1 (\\ c15 -> x2 c15 (\\ c17 -> x3 c17))\n\
\\n\
\  Djinn> callCC ? ((a -> C b) -> C a) -> C a\n\
\  callCC :: ((a -> C b) -> C a) -> C a\n\
\  callCC x1 x2 = x1 (\\ c15 _ -> x2 c15) (\\ c11 -> x2 c11)\n\
\\n\
\\n\
\  -- return and bind in the state monad\n\
\  Djinn> type S s a = (s -> (a, s))\n\
\  Djinn> returnS ? a -> S s a\n\
\  returnS :: a -> S s a\n\
\  returnS x1 x2 = (x1, x2)\n\
\  Djinn> bindS ? S s a -> (a -> S s b) -> S s b\n\
\  bindS :: S s a -> (a -> S s b) -> S s b\n\
\  bindS x1 x2 x3 = case x1 x3 of\n\
\                     (v4, v5) -> case x2 v4 x3 of\n\
\                                   (v7, v8) -> (v7, v5)\n\
\  -- NOTE: this is wrong; there are many ways to pass the state\n\
\  -- and Djinn picked a non-standard way.\n\
\\n\
\\n\
\Theory\n\
\======\n\
\  Djinn interprets a Haskell type as a logic formula using the\n\
\Curry-Howard isomorphism and then a decision procedure for\n\
\Intuitionistic Propositional Calculus.  This decision procedure is\n\
\based on Gentzen's LJ sequent calculus, but in a modified form, LJT,\n\
\that ensures termination.  This variation on LJ has a long history,\n\
\but the particular formulation used in Djinn is due to Roy Dyckhoff.\n\
\The decision procedure has been extended to generate a proof object\n\
\(i.e., a lambda term).  It is this lambda term (in normal form) that\n\
\constitutes the Haskell code.\n\
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
\  f x1 = x1 (Right (\\ c7 -> void (x1 (Left c7))))\n\
\"
