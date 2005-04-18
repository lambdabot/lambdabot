module Main where

import Plugins.EvalModule.LMEngine (evaluate, define)

#if __GLASGOW_HASKELL__ >= 604
import Test.HUnit
#else
import HUnit
#endif

-- import QuickCheck
-- import QuickCheckM 

import qualified Map (empty)


-- TODO: add tests for define, need to test List interactions (e.g. 1+head l)

main = runTestTT tests >>= print

tests = TestList 
    [
    test_Arith,
    test_Rel,
    test_List,
    test_Lambda,
    test_Misc,
    list_ender -- for rearranging and removing tests
    ]

list_ender = TestCase $ return ()

typeErrorMsg = "type error"
-- outOfFuelMsg = "out of fuel"
divideByZeroMsg = "divide by zero"
unboundVariableMsg = "unbound variable"
tailMsg = "tail of empty list"
headMsg = "head of empty list"

infFuel = -1
emptyEnv = Map.empty

-- no label
genericTest' :: String -> String -> Test
genericTest' expected code = expected ~=? case evaluate code emptyEnv infFuel of
                                            Left _ -> error "shouldn't get here"
                                            Right s -> s

-- labelled with code
genericTest :: String -> String -> Test
genericTest expected code = genericTestLbl code expected code

-- explicit label
genericTestLbl :: String -> String -> String -> Test
genericTestLbl lbl expected code = lbl ~: genericTest' expected code

-- The tests -------------------------------------------------------------------

-- Arith --

test_Arith = TestLabel "Arith" $ TestList
    [
    genericTest "-85" "4 + 3-2*(6+5*8)",
    genericTest divideByZeroMsg "1/0",
    genericTest "5" "5",
    list_ender
    ]

-- Lambda --

test_Lambda = TestLabel "Lambda" $ TestList
    [
    genericTest "10" "(\\x.x) 10",
    genericTest "5" "(\\x.x+3) 2",
    genericTestLbl "factorial" "120" "(\\f.(\\x.f(x x))(\\x.f(x x)))(\\fac n.if n == 0 then 1 else n*fac (n-1)) 5",
    genericTest "10" "(\\n.if n == 0 then n+3 else n+5) 5",
    genericTest "12" "(\\f.f 7) ((\\x y.x+y) 5)",
    genericTest "0" "(\\f.(\\x.f(x x))(\\x.f(x x)))(\\count n.if n==0 then 0 else count (n-1)) 1000",
    genericTest "15" "(\\S K I g f.(S (S (K S) (K I)) (S (K K) I)) g f) (\\f g x.f x (g x)) (\\r t.r) (\\l.l) 5 (\\m.m+10)",
    genericTest "14" "(\\x.x*2) $ 4+3",
    genericTest "-6" "((\\x.x*3) . (\\x.x-2)) 0",
    list_ender
    ]

-- Rel --

test_Rel = TestLabel "Rel" $ TestList
    [
    genericTest "True" "if 10 == 10 then True else False",
    genericTest "True" "5 == 5",
    genericTest "True" "5 /= 6",
    genericTest "False" "5 /= 5",
    genericTest "False" "5 == 6",
    genericTest "True" "5 <= 6",
    genericTest "False" "5 >= 6",
    genericTest "True" "5 < 6",
    genericTest "False" "5 > 6",
    genericTest "True" "5 == 10 || 6 == 6",
    genericTest "False" "5 == 10 && 6 == 6",
    genericTest "True" "True == True",
    genericTest "False" "'b' == 'c'",
    genericTest "True" "[] == []",
    genericTest "True" "[5,\"aoe\"] == [5,\"aoe\"]",
    genericTest "True" "null [] || 5 <= 6",
    genericTest "False" "\"aoeu\" == \"\"",
    genericTest "True" "1 == 1 || 1/0 == 1",
    genericTest "False" "1 == 0 && 1/0 == 1",
    list_ender
    ]

-- List --

test_List = TestLabel "List" $ TestList
    [
    genericTest "aoeu" "\"aoeu\"",
    genericTest "aoeu" "['a','o','e','u']",
    genericTest "a" "'a'",
    genericTest "[1, True, 3]" "[1,True,3]",
    genericTest "[1, True, 3]" "1:True:3:[]",
    genericTest "[True, 3]" "tail [1,True,3]",
    genericTest "1" "head [1,2,3]",
    genericTest tailMsg "tail []",
    genericTest headMsg "head []",
    genericTest "False" "null \"aoeu\"",
    genericTest "True" "null []",
    genericTest "True" "null \"\"",
    genericTest "[]" "[]",
    genericTest "1" "(\\f.f [1,2]) head",
    genericTestLbl "Tree" "(a (b c))" "(\\Leaf Branch tree.(\\f.(\\x.f(x x))(\\x.f(x x)))(\\showTree t.tree (\\l.[l]) (\\l r.'(':showTree l++' ':showTree r++\")\") t) (Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c')))) (\\c l b.l c) (\\l r lk bk.bk l r) (\\l b t.t l b)",
    list_ender
    ]

-- Misc --

test_Misc = TestLabel "Misc" $ TestList
    [
    test_TypeError,
    "ConsStrictness" ~: "1" ~=? case evaluate "head [1,(\\x.x x)(\\x.x x)]" emptyEnv 100 of Left _ -> "ConsStrictness";Right s -> s,
    "NullStrictness" ~: "False" ~=? case evaluate "null [1,(\\x.x x)(\\x.x x)]" emptyEnv 100 of Left _ -> "NullStrictness";Right s -> s,
    "ConsStrictness2" ~: "1" ~=? case evaluate "head ((\\f.(\\x.f(x x))(\\x.f(x x))) (\\x.1:x))" emptyEnv 100 of Left _ -> "ConsStrictness";Right s -> s,
--     "OutOfFuel" ~: (outOfFuelMsg ~=? evaluate "(\\x.x x) (\\x.x x)" emptyEnv 100),
    list_ender
    ]

test_TypeError = TestLabel "TypeError" $ TestList 
    [
    genericTest typeErrorMsg "(\\f.f 5) 7",
    genericTest typeErrorMsg "(\\x.x)+4",
    genericTest typeErrorMsg "if 4 then True else False",
    genericTest typeErrorMsg "null 5",
    list_ender
    ]

