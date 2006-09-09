-- Copyright 2006, Sascha Boehme.




-- List types of functions which are defined in Haskell libraries.

module PredefinedTypes where



-- Lists types of functions defined in Haskell libraries.

predefinedTypes :: [(String, String)]
predefinedTypes = preludeTypes



-- Lists the types of all polymorphic functions from the Haskell prelude.

preludeTypes :: [(String, String)]
preludeTypes =
  [ ("maybe", "forall a b. b -> (a -> b) -> Maybe a -> b")
  , ("either", "forall a b c. (a -> c) -> (b -> c) -> Either a b -> c")
  , ("fst", "forall a b. (a, b) -> a")
  , ("snd", "forall a b. (a, b) -> b")
  , ("curry", "forall a b c. ((a, b) -> c) -> a -> b -> c")
  , ("uncurry", "forall a b c. (a -> b -> c) -> (a, b) -> c")
  , ("id", "forall a. a -> a")
  , ("const", "forall a b. a -> b -> a")
  , ("(.)", "forall a b c. (b -> c) -> (a -> b) -> a -> c")
  , ("flip", "forall a b c. (a -> b -> c) -> b -> a -> c")
  , ("($)", "forall a b. (a -> b) -> a -> b")
  , ("until", "forall a. (a -> Bool) -> (a -> a) -> a -> a")
  , ("asTypeOf", "forall a. a -> a -> a")
  , ("error", "forall a. String -> a")
  , ("undefined", "forall a. a")
  , ("seq", "forall a b. a -> b -> b")
  , ("($!)", "forall a b. (a -> b) -> a -> b")
  , ("map", "forall a b. (a -> b) -> [a] -> [b]")
  , ("(++)", "forall a. [a] -> [a] -> [a]")
  , ("filter", "forall a. (a -> Bool) -> [a] -> [a]")
  , ("head", "forall a. [a] -> a")
  , ("last", "forall a. [a] -> a")
  , ("tail", "forall a. [a] -> [a]")
  , ("init", "forall a. [a] -> [a]")
  , ("null", "forall a. [a] -> Bool")
  , ("length", "forall a. [a] -> Int")
  , ("(!!)", "forall a. [a] -> Int -> a")
  , ("reverse", "forall a. [a] -> [a]")
  , ("foldl", "forall a b. (a -> b -> a) -> a -> [b] -> a")
  , ("foldl1", "forall a b. (a -> a -> a) -> [a] -> a")
  , ("foldr", "forall a b. (a -> b -> b) -> b -> [a] -> b")
  , ("foldr1", "forall a. (a -> a -> a) -> [a] -> a")
  , ("any", "forall a. (a -> Bool) -> [a] -> Bool")
  , ("all", "forall a. (a -> Bool) -> [a] -> Bool")
  , ("concat", "forall a. [[a]] -> [a]")
  , ("concatMap", "forall a b. (a -> [b]) -> [a] -> [b]")
  , ("scanl", "forall a b. (a -> b -> a) -> a -> [b] -> [a]")
  , ("scanl1", "forall a. (a -> a -> a) -> [a] -> [a]")
  , ("scanr", "forall a b. (a -> b -> b) -> b -> [a] -> [b]")
  , ("scanr1", "forall a. (a -> a -> a) -> [a] -> [a]")
  , ("iterate", "forall a. (a -> a) -> a -> [a]")
  , ("repeat", "forall a. a -> [a]")
  , ("replicate", "forall a. Int -> a -> [a]")
  , ("cycle", "forall a. [a] -> [a]")
  , ("take", "forall a. Int -> [a] -> [a]")
  , ("drop", "forall a. Int -> [a] -> [a]")
  , ("splitAt", "forall a. Int -> [a] -> ([a], [a])")
  , ("takeWhile", "forall a. (a -> Bool) -> [a] -> [a]")
  , ("dropWhile", "forall a. (a -> Bool) -> [a] -> [a]")
  , ("span", "forall a. (a -> Bool) -> [a] -> ([a], [a])")
  , ("break", "forall a. (a -> Bool) -> [a] -> ([a], [a])")
  , ("zip", "forall a b. [a] -> [b] -> [(a, b)]")
  , ("zip3", "forall a b c. [a] -> [b] -> [c] -> [(a, b, c)]")
  , ("zipWith", "forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]")
  , ("zipWith3", "forall a b c d. (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]")
  , ("unzip", "forall a b. [(a, b)] -> ([a], [b])")
  , ("unzip3", "forall a b c. [(a, b, c)] -> ([a], [b], [c])")
  , ("readParen", "forall a. Bool -> ReadS a -> ReadS a")
  ]
