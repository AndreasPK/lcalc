{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SuperConv where

import Parser (Var)
import Compiler
import CompTypes

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Monoid
import Control.Monad


type Combinator = Expression

newtype CombinatorList = CombinatorList [(Var, Combinator)] 
    deriving (Eq, Show, Ord, Monoid)

applyAll :: Expression -> [Expression] -> Expression
applyAll e arguments =
    foldl (\b a -> App b a) e arguments  

integrateCombinators :: Expression -> CombinatorList -> Expression
integrateCombinators e (CombinatorList pairs) =
    let (vars, exprs) = unzip pairs
    in
    applyAll e exprs  


{-
Supercombinator transformation.

Takes an expression, returns:
* A replacement for the expression
* A list of supercombinators and the variables
  by which we reference them in the replacement expression.
-}

makeSuper :: Expression -> CpM (Expression, CombinatorList)
makeSuper e = do
    (_, e', cs) <- makeSuper' e
    return (e',cs)
makeSuper' :: Expression -> CpM (Set Var, Expression, CombinatorList)
makeSuper' (App e1 e2) = do
    (b1, e1', c1) <- makeSuper' e1
    (b2, e2', c2) <- makeSuper' e2
    return (b1 <> b2, App e1' e2', c1 <> c2)
makeSuper' (Lambda vs (Lambda vs2 body)) =
    makeSuper' (Lambda (vs++vs2) body)
makeSuper' (Lambda vs body) = do
    res@(bodyVars, body', c1) <- makeSuper' body
    let freeVars = Set.toList $ Set.filter (\e -> not (elem e vs)) bodyVars :: [Var]
    replacements <- replicateM (length freeVars) newVar
    let newBody = substitute freeVars (map Variable replacements) body :: Expression
    if null freeVars 
        then 
            --Since there are no free variables we don't have to use the new body
            return res 
        else 
            let combinatored = 
                    applyAll 
                        (Lambda (vs ++ replacements) newBody)
                        (map Variable freeVars)
            in
            return (mempty, combinatored, mempty)
makeSuper' e@(Variable v) = return (Set.singleton v, e, mempty)
makeSuper' e = return (mempty, e, mempty)


