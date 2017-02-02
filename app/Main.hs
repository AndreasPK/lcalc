module Main where

import Text.ParserCombinators.ReadP as ReadP
import Lib (Expression(..), parseCode, Var)
import Interpreter
import Debug.Trace
import Data.Function
import Data.Maybe
--import Control.Exception.Base


hoftest = "(\\f -> (\\x -> f x) ) (\\x -> x) 1" -- == 1
subtest = "(\\x -> (\\x -> 1 + x))"
yapp = "(\\f -> (\\x -> x x) (\\x -> f (\\v -> (x x) v ) ) )"
y2 = "(\\h -> (\\x -> h (x x)) (\\x -> h (x x)))"
yCombinator = "(\\f -> (\\x -> f (x x))) (\\f -> (\\x -> f (x x))))"

my = parseCode "(\\f -> (\\x -> f (x x)) (\\x -> f (x x)) )"
mz = parseCode "(\\f -> (\\x -> f (\\y -> x x y)) (\\x -> f (\\y -> x x y)) )"


rfac = "(\\f -> (\\n -> if (n == 0) (1) (n * (f f (n-1))))) "
crfac = parseCode rfac
c1 = parseCode "1"
comb =
  "(\\f -> (\\x -> f x x)) (\\f -> (\\x -> f x x)))" ++
  "(\\f n -> if (n == 0) (1) (n * (f f (n-1))))" ++
  "1"


main :: IO ()
main = putStrLn "Test"
