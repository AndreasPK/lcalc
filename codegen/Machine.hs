module Machine where

import qualified Data.Sequence as Seq


builtinArity "if" = 3
builtinArity "==" = 2
builtinArity "/=" = 2
builtinArity "*" = 2
builtinArity "/" = 2
builtinArity "+" = 2
builtinArity "add" = 2
builtinArity "-" = 2


data ByteCode 
    = OP_BREAK
    | OP_HALT
    | OP_ADD
    | OP_IF
    | OP_EQ
    | OP_NEQ
    | OP_MULT
    | OP_DIV
    | OP_SUB
    | OP_PUSH Int --From where?
    deriving (Eq, Ord, Show)

data HeapObject = Value Int deriving (Eq, Show, Ord)

data Function = Code [ByteCode] deriving (Eq, Show, Ord)


newtype DataStack = DataStack (Seq.Seq HeapObject) deriving (Eq, Ord, Show)
newtype ContStack = ContStack (Seq.Seq Function)  deriving (Eq, Ord, Show)


data MachineState = MachineState 
    { 
        
    } deriving (Eq, Show, Ord)