module Main where

import Text.ParserCombinators.ReadP as ReadP
import Parser (Expression(..), parseCode, Var)
import qualified Interpreter as I
import Debug.Trace
import Data.Function
import Data.Maybe
import Compiler
import Control.Monad
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
comb = "(\\f x -> if (x == 1) 1 ((x) * (f f (x-1)))) " ++
       "(\\f x -> if (x == 1) 1 (x * (f f (x-1)))) " ++
       "2"

rc :: String -> Compiler.Expression
rc s =
  let c = compile . parseCode $ s
  in
  toWhnf c

repl :: IO ()
repl = do
  input <- getLine
  unless (input `elem` ["quit", "q", "exit"]) $ do
    let result = compile . parseCode $ input
    print result
    putStrLn $ "Value:" ++ show (toWhnf result)
    repl 

main :: IO ()
main = repl
