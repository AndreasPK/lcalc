{-|
Module      : Lib
Description : Compile language into lambda calculus equivalents
Copyright   : (c) Andreas Klebinger, 2017
License     : GPL-3
Stability   : experimental
Portability :

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}



module Compiler where

import qualified Parser (Expression(..), Var, parseCode)
import Parser (Var)
import Data.Maybe
import qualified Data.Sequence as S
import GHC.Generics
import Test.QuickCheck
import Control.Applicative
import Debug.Trace
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Monoid


data Expression =
  Value Int |
  Lambda Var Expression |
  App Expression Expression |
  Variable Var |
  Builtin String |
  Bottom
  deriving (Eq, Show, Generic)

instance Pretty Expression where
  pretty (Value i) = pretty i
  pretty (Lambda var expr) = parens ("\\" <> pretty var <+> "->" <+> nest 4 ( softline <> pretty expr))
  pretty (App inner@(App e1 e2) e3) = pretty inner <+> pretty e3
  pretty (App e1@(Lambda {}) e2) = pretty e1 <+> pretty e2
  pretty (App e1 e2) = parens (pretty e1 <+> pretty e2)
  pretty (Variable v) = pretty v
  pretty (Builtin name) = pretty name
  pretty (Bottom) = "__|__"


instance Arbitrary Expression where
  arbitrary = oneof
    [ Value <$> arbitrary
    , Lambda <$> arbitrary <*> arbitrary
    , App <$> arbitrary <*> arbitrary
    , Variable <$> arbitrary
    , Builtin <$> arbitrary
    ]
  shrink e = []

esize Value {} = 1
esize (Lambda var body) = 1 + esize body
esize (App f arg) = 1 + esize f + esize arg
esize Variable {} = 1
esize Builtin {} = 1


--data StackEntry = SEntry Expression deriving (Show, Eq)

substitute :: Var -> Expression -> Expression -> Expression
substitute vname expr e@(Value v) = e
substitute vname expr e@(Lambda var body)
  | vname == var = e
  | otherwise    = Lambda var (substitute vname expr body)
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
builtinArity "add" = 2
builtinArity "-" = 2
builtinArity e = error $ "Not a builtin function:" ++ show e

getArity :: Expression -> Int
getArity (Lambda _ _) = 1
getArity (Builtin name) = builtinArity name
getArity e = error $ "Can't get arity for non applicable element: " ++ show e

{-
  One evaluation step consists of:
  Finding the next redex
  Either reducing it directly or:
  Checking it's arity, collecting the parameters and then executing it.
-}

reduce :: Expression -> Expression
--reduce (App (Lambda var body) arg) = substitute var arg body
reduce e@(App _ _)  =
  let spine = linearizeSpine e
      op = last spine
      arity = getArity op
      (args,stack) = splitAt arity $ tail . reverse $ spine
      result = instantiate op args
  in  traceShow ((reverse stack ++ [result])) $
    retree (reverse stack ++ [result])

reduce e = e

retree :: [Expression] -> Expression
retree [] = undefined
retree [a] = a
retree [a,f] = App f a
retree (a1:as) = App (retree as) a1

instantiate :: Expression -> [Expression] -> Expression
instantiate (Lambda var body) args = substitute var (head args) body
instantiate (Builtin name) args = evalBuiltin name args



isWhnf :: Expression -> Bool
isWhnf Value {} = True
isWhnf Lambda {} = True
isWhnf Builtin {} = True
isWhnf e@(App _ _) =
  let spine = linearizeSpine e
      arity = getArity . last $ spine
      argCount = length spine - 1
  in
  arity > argCount

toWhnf :: Expression -> Expression
toWhnf e
  | isWhnf e = e
  | otherwise = toWhnf (reduce e)

eqfix :: Eq a => (a->a)->a->a
eqfix f a =
 if f a == a then a
   else eqfix f (f a)

-- | Transform the spine into a list
-- f a b -> [b,a,f]
linearizeSpine :: Expression -> [Expression]
linearizeSpine (App ( a@(App _ _)) arg) = arg : linearizeSpine a
linearizeSpine (App f arg) = [arg, f]
linearizeSpine e = [e]

unwind :: Expression -> [Expression]
unwind (App f arg) = undefined
unwind e = [e]


evalBuiltin :: String -> [Expression] -> Expression
evalBuiltin name args =
  case name of
    "if" -> case toWhnf (head args) of
      Value 0 -> args !! 2
      Value _ -> args !! 1
      _ -> error $ "If condition not reducing to 0 or 1:" ++ show (head args)
    "==" -> case map toWhnf args of
      [Value v1, Value v2]
        | v1 == v2 -> Value 1
        | otherwise -> Value 0
      _ -> error $ "Can't compare non value types" ++ show args
    "/=" -> case map toWhnf args of
      [Value v1, Value v2]
        | v1 == v2 -> Value 0
        | otherwise -> Value 1
      _ -> error $ "Can't compare non value types" ++ show args
    "*" -> numop (*)
    "/" -> numop div
    "+" -> numop (+)
    "add" -> numop (+)
    "-" -> numop (-)
    _ -> error "Unknown builtin function"
    where
      numop :: (Int -> Int -> Int) -> Expression
      numop f = case map toWhnf args of
        [Value v1, Value v2] -> Value $ f v1 v2
        _ -> error $ "Failed to apply builtin, arguments not two numbers" ++ show args
