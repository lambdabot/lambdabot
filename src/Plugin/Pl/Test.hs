{-# LANGUAGE CPP #-}
module Main where

#define READLINE
#if __GLASGOW_HASKELL__ > 602
import Test.HUnit
import Test.QuickCheck hiding (test)
#else
import HUnit
import Debug.QuickCheck hiding (test)
#endif

import Plugin.Pl.Common
import Plugin.Pl.Transform
import Plugin.Pl.Parser
import Plugin.Pl.PrettyPrinter

import Data.List ((\\))
import Data.Char (isSpace)

import Control.Monad.Error

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment (getArgs)

#ifdef READLINE
import System.Console.Readline (readline, addHistory, initialize)
#endif

import Debug.Trace

instance Arbitrary Expr where
  arbitrary = sized $ \size -> frequency $ zipWith (,) [1,size,size]
    [arbVar,
     liftM2 Lambda arbPat arbitrary,
     let se = resize (size `div` 2) arbitrary in liftM2 App se se ]
  coarbitrary = error "Expr.coarbitrary"

arbVar :: Gen Expr
arbVar = oneof [(Var Pref . return) `fmap` choose ('a','z'),
                (Var Inf .  return) `fmap` elements (opchars\\"=")]

arbPat :: Gen Pattern
arbPat = sized $ \size ->
  let
    spat = resize (size `div` 5) arbPat
  in
    frequency $ zipWith (,) [1,size,size] [
      (PVar . return) `fmap` choose ('a','z'),
      liftM2 PTuple spat spat,
      liftM2 PCons  spat spat]

propRoundTrip :: Expr -> Bool
propRoundTrip e = Right (TLE e) == parsePF (show e)

-- hacking qc2 functionality (?) in here
propRoundTrip' :: Expr -> Property
propRoundTrip' e = not (propRoundTrip e) ==> trace (show $ findMin e) False
    where
  findMin e' = case filter (not . propRoundTrip) $ subExpr e' of
    [] -> e'
    (x:_) -> findMin x

propMonotonic1 :: Expr -> Expr -> Expr -> Bool
propMonotonic1 e e1 e2 = App e e1 `compare` App e e2 == e1 `compare` e2

propMonotonic2 :: Expr -> Expr -> Expr -> Bool
propMonotonic2 e e1 e2 = App e1 e `compare` App e2 e == e1 `compare` e2

subExpr :: Expr -> [Expr]
subExpr (Var _ _) = []
subExpr (Lambda v e) = [e] ++ Lambda v `map` subExpr e
subExpr (App e1 e2) = [e1, e2]
  ++ App e1 `map` subExpr e2 ++ (`App` e2) `map` subExpr e1
subExpr (Let {}) = bt

sizeTest :: IO ()
sizeTest = quickCheck $ \e -> collect (sizeExpr e) (propRoundTrip e)

quick :: Config
quick = Config
  { configMaxTest = 100
  , configMaxFail = 1000
  , configSize    = const 40
  , configEvery   = \n _ -> let sh = show n in sh ++ [ '\b' | _ <- sh ]
  }

myTest :: IO ()
myTest = check quick propRoundTrip'

qcTests :: IO ()
qcTests = do
  quickCheck propRoundTrip
  quickCheck propMonotonic1
  quickCheck propMonotonic2

pf :: String -> IO ()
pf inp = case parsePF inp of
  Right d -> do
    putStrLn "Your expression:"
    print d
    putStrLn "Transformed to pointfree style:"
    let d' = mapTopLevel transform d
    print $ d'
    putStrLn "Optimized expression:"
    mapM_ print $ mapTopLevel' optimize d'
  Left err -> putStrLn $ err

mapTopLevel' :: Functor f => (Expr -> f Expr) -> TopLevel -> f TopLevel
mapTopLevel' f tl = case getExpr tl of (e, c) -> fmap c $ f e

pf' :: String -> IO ()
pf' = putStrLn . (id ||| show) . parsePF

-- NB: this is a special case of (import Control.Monad.Reader)
-- ap :: m (a -> b) -> m a -> m b
s :: (t -> a -> b) -> (t -> a) -> t -> b
s f g x = f x $ g x

unitTest :: String -> [String] -> Test
unitTest inp out = TestCase $ do
  d <- case parsePF inp of
    Right x -> return x
    Left err -> fail $ "Parse error on input " ++ inp ++ ": " ++ err
  let d' = mapTopLevel (last . optimize . transform) d
  foldr1 mplus [assertEqual (inp++" failed.") o (show d') | o <- out]

unitTests :: Test
unitTests = TestList [
  unitTest "foldr (++) []" ["join"],
  unitTest "flip flip [] . ((:) .)" ["(return .)"],
  unitTest "\\x -> x - 2" ["subtract 2"],
  unitTest "\\(x,_) (y,_) -> x == y" ["(. fst) . (==) . fst"],
  unitTest "\\x y z -> return x >>= \\x' -> return y >>= \\y' -> return z >>= \\z' -> f x' y' z'" ["f"],
  unitTest "let (x,y) = (1,2) in y" ["2"],
  unitTest "fix . const" ["id"],
  unitTest "all f . map g" ["all (f . g)"],
  unitTest "any f . map g" ["any (f . g)"],
  unitTest "liftM2 ($)" ["ap"],
  unitTest "\\f -> f x" ["($ x)"],
  unitTest "flip (-)" ["subtract"],
  unitTest "\\xs -> [f x | x <- xs, p x]" ["map f . filter p"],
  unitTest "all id" ["and"],
  unitTest "any id" ["or"],
  unitTest "and . map f" ["all f"],
  unitTest "or . map f" ["any f"],
  unitTest "return ()" ["return ()"],
  unitTest "f (fix f)" ["fix f"],
  unitTest "concat ([concat (map h (k a))])" ["h =<< k a"],
  unitTest "uncurry (const f)" ["f . snd"],
  unitTest "uncurry const" ["fst"],
  unitTest "uncurry (const . f)" ["f . fst"],
  unitTest "\\a b -> a >>= \\x -> b >>= \\y -> return (x,y)" ["liftM2 (,)"],
  unitTest "\\b a -> a >>= \\x -> b >>= \\y -> return (x,y)" ["flip liftM2 (,)"],
  unitTest "curry snd" ["const id"],
  unitTest "\\x -> return x y" ["const y"],
  unitTest "\\x -> f x x" ["join f"],
  unitTest "join (+) 1" ["2"],
  unitTest "fmap f g x" ["f (g x)"],
  unitTest "liftM2 (+) f g 0" ["f 0 + g 0", "g 0 + f 0"],
  unitTest "return 1 x" ["x"],
  unitTest "f =<< return x" ["f x"],
  unitTest "(=<<) id" ["join"],
  unitTest "zipWith (,)" ["zip"],
  unitTest "map fst . zip [1..]" ["zipWith const [1..]"],
  unitTest "curry . uncurry" ["id"],
  unitTest "uncurry . curry" ["id"],
  unitTest "curry fst" ["const"],
  unitTest "return x >> y" ["y"],
  -- What were they smoking when they decided >> should be infixl
  unitTest "a >>= \\_ -> b >>= \\_ -> return $ const (1 + 2) $ a + b" ["a >> (b >> return 3)"],
  unitTest "foo = m >>= \\x -> return 1" ["foo = m >> return 1"],
  unitTest "foo m = m >>= \\x -> return 1" ["foo = (>> return 1)"],
  unitTest "return (+) `ap` return 1 `ap` return 2" ["return 3"],
  unitTest "liftM2 (+) (return 1) (return 2)" ["return 3"],
  unitTest "(. ((return .) . (+))) . (>>=)" ["flip (fmap . (+))"],
  unitTest "\\a b -> a >>= \\x -> b >>= \\y -> return $ x + y" ["liftM2 (+)"],
  unitTest "ap (flip const . f)" ["id"],
  unitTest "uncurry (flip (const . flip (,) (snd t))) . ap (,) id" ["flip (,) (snd t)"],
  unitTest "foo = (1, fst foo)" ["foo = (1, 1)"],
  unitTest "foo = (snd foo, 1)" ["foo = (1, 1)"],
  unitTest "map (+1) [1,2,3]" ["[2, 3, 4]"],
  unitTest "snd . (,) (\\x -> x*x)" ["id"],
  unitTest "return x >>= f" ["f x"],
  unitTest "m >>= return" ["m"],
  unitTest "m >>= \\x -> f x >>= g" ["m >>= f >>= g", "g =<< f =<< m"],
  unitTest "\\x -> 1:2:3:4:x" ["([1, 2, 3, 4] ++)"],
  unitTest "\\(x:xs) -> x"  ["head"],
  unitTest "\\(x:xs) -> xs" ["tail"],
  unitTest "\\(x,y)  -> x"  ["fst"],
  unitTest "\\(x,y)  -> y"  ["snd"],
  unitTest "\\x -> x" ["id"],
  unitTest "\\x y -> x" ["const"],
  unitTest "\\f x y -> f y x" ["flip"],
  unitTest "t f g x = f x (g x)" ["t = ap"],
  unitTest "(+2).(+3).(+4)" ["(9 +)"],
  unitTest "head $ fix (x:)" ["x"],
  unitTest "head $ tail $ let xs = x:ys; ys = y:ys in xs" ["y"],
  unitTest "head $ tail $ let ys = y:ys in let xs = x:ys in xs" ["y"],
  unitTest "2+3*4-3*3" ["5"],
  unitTest "foldr (+) x [1,2,3,4]" ["10 + x", "x + 10"],
  unitTest "foldl (+) x [1,2,3,4]" ["10 + x", "x + 10"],
  unitTest "head $ fst (x:xs, y:ys)" ["x"],
  unitTest "snd $ (,) 2 3" ["3"],
  unitTest "\\id x -> id" ["const"],
  unitTest "\\y -> let f x = foo x; g = f in g y" ["foo"],
  unitTest "neq x y = not $ x == y" ["neq = (/=)"],
  unitTest "not (x /= y)" ["x == y"],
  unitTest "\\x x -> x" ["const id"],
  unitTest "\\(x, x) -> x" ["snd"],
  unitTest "not $ not 4" ["4"],
  unitTest "\\xs -> foldl (+) 0 (1:2:xs)" ["foldl (+) 3"],
  unitTest "\\x -> foldr (+) x [0,1,2,3]" ["(6 +)"],
  unitTest "foldr (+) 0 [x,y,z]" ["x + y + z"],
  unitTest "foldl (*) 0 [x,y,z]" ["0"],
  unitTest "length \"abcdefg\"" ["7"],
  unitTest "ap (f x . fst) snd" ["uncurry (f x)"],
  unitTest "sum [1,2,3,x]" ["6 + x", "x + 6"],
  unitTest "p x = product [1,2,3,x]" ["p = (6 *)"],
  unitTest "(concat .) . map" ["(=<<)"],
  unitTest "let f ((a,b),(c,d)) = a + b + c + d in f ((1,2),(3,4))" ["10"],
  unitTest "let x = const 3 y; y = const 4 x in x + y" ["7"] -- yay!
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ("tests":_) -> doTests
    xs          -> do
        mapM_ pf xs
#ifdef READLINE
        initialize
#endif
        pfloop


pfloop :: IO ()
pfloop = do
#ifdef READLINE
  line' <- readline "pointless> "
#else
  line' <- Just `fmap` getLine
#endif
  case line' of
    Just line
      | all isSpace line -> pfloop
      | otherwise        -> do
#ifdef READLINE
          addHistory line
#endif
          pf line
          pfloop
    Nothing   -> putStrLn "Bye."

doTests :: IO ()
doTests = do
  runTestTT unitTests
--  qcTests
  return ()
