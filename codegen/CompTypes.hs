module CompTypes where

import Parser (Var)
import Compiler

import Control.Monad.State.Strict

data CompilerState 
    = CompState 
    { lastUnique :: Int }
    deriving (Eq, Show, Ord)

type CpM = State CompilerState

type Vars = [Var]

runCpM f = runState f (CompState { lastUnique = 0 })

nextUnique :: CpM Int
nextUnique = do
    s <- get
    let newState = s { lastUnique = lastUnique s + 1}
    put $ newState
    return $ lastUnique newState

newVar :: CpM Var
newVar = do
    i <- nextUnique
    return $ "__var_" ++ show i