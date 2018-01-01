module Gen where

import Compiler
import Control.Monad.State.Strict
import Parser (Var)

import CompTypes



{-}
data Expression =
  Value Int |
  Lambda Var Expression |
  App Expression Expression |
  Variable Var |
  Builtin String |
  Bottom
  -}


varOccs :: Expression -> [Var]
varOccs (Lambda vs body)  = vs ++ varOccs body
varOccs (App e1 e2)      = varOccs e1 ++ varOccs e2
varOccs (Variable v)     = [v]
varOccs _                = []

freeVars :: Expression -> [Var]
freeVars (Lambda v body) = undefined

