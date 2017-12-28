{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Interpreter where

{-
  A simple interpreter for a (not completely correct) implementation of Lambda
  calculus. It is based on beta reduction, however it does not work correctly
  when evaluating partial applications.

-}

import Text.ParserCombinators.ReadP as ReadP
import Parser (Expression(..), parseCode, Var)
import Debug.Trace
import Data.Function
import Data.Maybe
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Monoid

instance Pretty Expression where
  pretty (Value x) = pretty x
  pretty (Lambda var body) = parens ("\\" <> maybe mempty pretty var <> " -> " <> pretty body)
  pretty (App f arg) = parens $ pretty f <> " " <> maybe "" pretty arg
  pretty (Variable name) = pretty name
  pretty (Let vname expr body) = parens ("let " <> pretty vname <> " = " <> pretty expr <> " in " <> pretty body)
  pretty (Builtin name args ) = parens (pretty name <> " " <> foldr1 (<+>) (map pretty args))
  pretty (Op e1 op e2) = parens (pretty e1 <> " " <> pretty op <> " " <> pretty e2)
  pretty Bottom = parens "_|_"

{-}
instance Show Expression where
  show (Value x) = "val:" ++ show x
  show (Lambda vars body) = pb $ "lam:\\" ++ show vars ++ " -> " ++ show body
  show (App f Nothing) = pb $ "app: " ++ show f
  show (App f (Just arg)) = pb $ "app: " ++ show f ++ " " ++ show arg
  show (Variable name) = "vn:" ++ name
  show (Let vname expr body) = "let: " ++ vname ++ " = " ++ show expr ++ " in " ++ show body
  show (Builtin name args ) = pb $ "builtin:" ++ show name ++ " " ++ show args
  show (Op e1 op e2) = pb $ "op:" ++ show e1 ++ " " ++ op ++ " " ++ show e2
  show Bottom = pb "_|_" -}

-- |Substitute a expression for a variable, does not substitute bound variables.
substitute :: Var -> Expression -> Expression -> Expression
substitute var e x@(Value _) = x
substitute var e x@(Lambda Nothing body) = Lambda Nothing (substitute var e body)
-- If v is bound inside the body no beta reductions will take place for the body
substitute var e x@(Lambda (Just v) body) =  if v == var then x else Lambda (Just v) $ substitute var e body
substitute var e (App f arg) = App (substitute var e f) (fmap (substitute var e) arg)
substitute var e (Op l op r) = Op (substitute var e l) op (substitute var e r)
substitute var e (Builtin s args) = Builtin s $ map (substitute var e) args
substitute var e (Let name def body) = Let name (substitute var e def) (substitute var e body)
substitute var e x@(Variable v) = if v == var then e else x

--simplify reduces complexity of the parsed expression without changing it semantis.
--basically a workaround for the badness of the parser
simplify :: Expression -> Expression
simplify (App (Value x) Nothing) = Value x
simplify (App e@(App _ _) Nothing) = simplify e
simplify (App e@Op {} Nothing) = simplify e
simplify (App f arg) = App (simplify f) $ fmap simplify arg
simplify (Lambda vars body) = Lambda vars $ simplify body
simplify (Op l o r) = Op (simplify l) o (simplify r)
simplify (Builtin name args) = Builtin name $ map simplify args
simplify (Let name def body) = Let name (simplify def) (simplify body)
simplify e@(Variable _ ) = e
simplify e@(Value _) = e
simplify e = e


toVal :: Expression -> Expression
toVal (Value x) = Value x
toVal e = toVal $ eval e

toFunc :: Expression -> Expression
toFunc e@(App _ _) = e
toFunc e@(Lambda _ _) = e
toFunc e = toFunc $ eval e

-- | Builtins and operators can't be partially applied in this Interpreter
evalBuiltin :: Expression -> Expression
evalBuiltin (Builtin "add" (e1:e2:xs)) = Value $ val e1 + val e2
evalBuiltin (Builtin "sub" (e1:e2:xs)) = Value $ val e1 - val e2
evalBuiltin (Builtin "if" [cond,t,f,x]) =
  let result = if val (eval' cond) /= 0 then App t else App f
  in result $ Just x
evalBuiltin (Builtin "if" [cond,t,f]) =
  if val (eval' cond) /= 0 then t else f

evalBuiltin e@(Builtin f _) = error $ "unknown function" ++ show e

evalOp :: Expression -> Expression
evalOp (Op l "+" r) = Value $ val (eval' l) + val (eval' r)
evalOp (Op l "-" r) = Value $ val (eval' l) - val (eval' r)
evalOp (Op l "/" r) = Value $ div (val (eval' l)) (val (eval' r))
evalOp (Op l "*" r) = Value $ val (eval' l) * val (eval' r)
evalOp (Op l "==" r) = Value $ if val (eval' l) == val (eval' r) then 1 else 0
evalOp (Op l "/=" r) = Value $ if val (eval' l) == val (eval' r) then 0 else 1
evalOp (Op l op r) = error $ "undefined operator op" ++ op

-- | Evaluates the given expression one step towards a value
eval :: Expression -> Expression
eval (App e@(Lambda vars body) Nothing ) = e
eval (App e Nothing) = e
eval e@(App _ _) =
  evalapp e
eval (Value x) = Value x
eval (Let var e body) = substitute var e body
eval e@LetRec {} =
  evalLetRec e
--eval (Lambda Nothing body) = body
eval (Lambda _ body) = body
eval e@(Builtin _ _) = evalBuiltin e
eval e@Op {} = evalOp e
eval e@(Variable name) = Variable name
--eval x = error $ "Expression should have been replaced by beta reduction: " ++ show x


evalLetRec :: Expression -> Expression
evalLetRec (LetRec var def body) =
  app (Lambda (Just var) body) (app y (Lambda (Just var) def))
  where
    y = simplify $ parseCode "(\\f -> (\\x -> f (x x)) (\\x -> f (x x)))"

app :: Expression -> Expression -> Expression
app f arg = App f $ Just arg

apps :: Expression -> [Expression] -> Expression
apps = foldl (\e a -> App e (Just a))

efix :: Expression -> Expression
efix e = if eval e == e
            then e
            else efix $ eval e

eval' e@Value {} = e
eval' e = efix e

run = eval' . parseCode

findFree :: Expression -> [String]
findFree (Value _) = []
findFree (Lambda (Just var) body) = filter (/= var) $ findFree body
findFree (Lambda Nothing body) = findFree body
findFree (App f a) = findFree f ++ maybe [] findFree a
findFree (Variable v) = [v]
findFree (Let v def body) = findFree def ++ filter (/= v) (findFree body)
findFree (LetRec v def body) = findFree def ++ filter (/= v) (findFree body)
findFree (Builtin name es) = concatMap findFree es
findFree (Op e1 op e2) = findFree e1 ++ findFree e2


{-
  Value { val :: Int } |
  Lambda { lvars :: Maybe Var, lbody :: Expression} |
  App {func :: Expression, arg :: Maybe Expression} |
  Variable { name :: Var } |
  Let Var Expression Expression |
  LetRec Var Expression Expression |
  Builtin String [Expression] |
  Op Expression String Expression |
-}

isFunction :: Expression -> Bool
isFunction Lambda {} = True
isFunction App {} = True
isFunction Builtin {} = True
isFunction _ = False

evalapp :: Expression -> Expression
evalapp (App (Lambda Nothing body) Nothing) = body
evalapp e@(App (Lambda (Just _) body) Nothing) = error $ "retarded application: " ++ show e
evalapp (App (Lambda (Just v) body) (Just arg)) =
  substitute v arg body
evalapp e@(App (Lambda Nothing body) arg) =
  case body of
    Lambda {} -> App body arg
    App {}-> App body arg
    Builtin {} -> App body arg
    _ -> error $ "Error? " ++ show e
evalapp e@(App (Value v) args)
  | null args = Value v
  | otherwise = error $ "Value expression can't be applied to arguments" ++ show e
evalapp (App f args) =
  App (eval f) args
