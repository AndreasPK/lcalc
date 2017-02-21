{-|
Module      : Lib
Description : Compile language into lambda calculus equivalents
Copyright   : (c) Andreas Klebinger, 2017
License     : GPL-3
Stability   : experimental
Portability :

-}

module Compiler where

import qualified Parser (Expression(..), Var, parseCode)
import Parser (Var)
import Data.Maybe


data Expression =
  Value Int |
  Lambda Var Expression |
  App Expression Expression |
  Variable Var |
  Builtin String
  deriving (Eq, Show)

substitute :: Var -> Expression -> Expression -> Expression
substitute vname expr e@(Value v) = e
substitute vname expr (Lambda var body) = Lambda var (substitute vname expr body)
substitute vname expr (App f arg) = App (substitute vname expr f) (substitute vname expr arg)
substitute vname expr e@(Variable v) = if v == vname then expr else e
substitute vname expr e@(Builtin n) = e

compile :: Parser.Expression -> Expression
compile (Parser.Value x) = Value x
compile (Parser.Lambda var body) = Lambda (fromMaybe "" var) (compile body)
compile (Parser.App f arg) =
  case arg of
    Nothing -> compile f
    Just v -> App (compile f) (compile v)
compile (Parser.Variable v) = Variable v
compile (Parser.Let name def body) = App (Lambda name (compile body)) (compile def)
compile (Parser.LetRec name def body) =
  App (Lambda name (compile body)) (App y (Lambda name (compile def)))
  where
    y = compile $ Parser.parseCode "(\\f -> (\\x -> f (x x)) (\\x -> f (x x)))"
compile (Parser.Builtin name args) = foldl App (Builtin name) $ map compile args
compile (Parser.Op e1 name e2) = App (App (Builtin name) (compile e1)) (compile e2)

builtinArity :: String -> Int
builtinArity "if" = 3
builtinArity "==" = 2
builtinArity "/=" = 2
builtinArity "*" = 2
builtinArity "/" = 2
builtinArity "+" = 2
builtinArity "-" = 2
builtinArity e = error $ "Not a builtin function:" ++ show e

{-
  One evaluation step consists of:
  Finding the next redex
  Either reducing it directly or:
  Checking it's arity, collecting the parameters and then executing it.
-}
eval :: Expression -> Expression
eval (App (Lambda var body) arg) = substitute var arg body
eval e@(App (Builtin _) _) = evalBuiltin e
eval e = e

unwind :: Expression -> [Expression]
unwind (App f arg) = undefined
unwind e = [e]


evalBuiltin :: Expression -> Expression
evalBuiltin (App (Builtin name) args) =
  let arity = builtinArity name
  in
  undefined
