import Test.HUnit
import Parser
import Compiler

rc :: String -> Compiler.Expression
rc = toWhnf . compile . parseCode

makeTest :: String -> String -> Int -> Test
makeTest testName code result =
  TestCase (
    assertEqual testName
    (Compiler.Value result)
    (rc code)
  )

substitionTest1 =
  TestCase (
    assertEqual "substitution"
    (rc "(\\x -> (\\x -> 1 + x) x) 1")
    (rc "(\\x -> (\\y -> 1 + y) x) 1")
  )

higherOrderTest1 = makeTest "HigherOrder1" "(\\f -> (\\x -> f x) ) (\\x -> x) 1" 1
multiApply = makeTest "MultApply" "(\\a -> a) (\\b -> b) (\\c -> 2) 1" 2
app2 = makeTest "app2" "(\\f x y-> f (x y)) (\\b -> 1 + b) (\\y -> y * 2) 1 " 3
multiVar = makeTest "MultiVar" "(\\a b c -> c) 1 2 3" 3
recursion = makeTest "recursion" "(\\f x -> if (x == 1) 1 (f f (x-1))) (\\f x -> if (x == 1) 1 (f f (x-1))) 4" 1
recursion2 = makeTest "recursion2" "(\\f x -> if (x == 1) 1 ((x) * (f f (x-1)))) (\\f x -> if (x == 1) 1 (x * (f f (x-1)))) 3" 6

h = "(\\fac -> \\n -> if (n == 0) 1 (n * (fac (n - 1))))"
y = "(\\f -> (\\x -> f (x x)) (\\x -> f (x x)))"

ytest =
  makeTest
    "Y-Cobinator"
    ("(" ++ y ++ " " ++ h ++ ")" ++ " 4" )
    24

testBuiltin = TestList
  [ makeTest "builtin:==" "1==1" 1
  , makeTest "builtin:==" "1==2" 0
  , makeTest "builtin:/=" "1/=2" 1
  , makeTest "builtin:if" "if 1 2 3" 2
  , makeTest "builtin:add" "add 1 2" 3
  ]

testLets = TestList
  [ makeTest "let" "let x = 1 in x + x" 2
  , makeTest "letrec" "letrec f = (\\x -> if (x == 0) 0 (f (x-1))) in f 1" 0
  ]

testShadowing = TestList 
  [ makeTest "shadow lambda" "(\\x -> (\\x -> x) 1) 3" 1
  , makeTest "shadow let"    "let x = 1 in (let x = 2 in x)" 2
  ]



main :: IO ()
main = do
  fails <- runTestTT $ TestList 
      [substitionTest1
      , higherOrderTest1
      , multiApply
      , multiVar
      , app2
      , ytest
      , recursion
      , recursion2
      , testBuiltin
      , testLets
      , testShadowing
      ]

  print fails
