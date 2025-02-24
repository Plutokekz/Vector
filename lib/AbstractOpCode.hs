module AbstractOpCode where

import Ast qualified
import Control.Monad.State.Lazy
import Data.Map (Map)
import Data.Map qualified as Map

data Instruction
  = RST
  | LOD Integer Integer
  | STO Integer Integer
  | LODN Integer Integer Integer -- Load n bytes from stack
  | LODO Integer Integer -- Loads 8bytes from stack but addes top of the stack to offset ussed to acces vector and matrix elements by varibes
  | STON Integer Integer Integer -- store n bytes on the stack
  | INC Integer
  | LIT Integer -- Load imidieate integer
  | LITV [Integer] -- Load imidieate Vector or Matrix
  | JMP String
  | JOT String
  | JOF String
  | CAL Integer String
  | RET
  | OPR Operator
  | REA
  | WRI
  | LAB String
  | HLT
  deriving (Show, Eq)

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Lte
  | Gt
  | Gte
  | Not
  | MatrixMul Dimension Dimension
  | MatrixAdd Dimension Dimension
  | MatrixSub Dimension Dimension
  | MatrixDiv Dimension Dimension
  | MatrixDivScalar Dimension
  | MatrixAddScalar Dimension
  | MatrixMulScalar Dimension
  | MatrixSubScalar Dimension
  | ScalarDivMatrix Dimension
  | ScalarAddMatrix Dimension
  | ScalarMulMatrix Dimension
  | ScalarSubMatrix Dimension
  | VectorMul Length
  | VectorAdd Length
  | VectorSub Length
  | VectorDiv Length
  | VectorAddScalar Length
  | VectorMulScalar Length
  | VectorDivScalar Length
  | VectorSubScalar Length
  | ScalarAddVector Length
  | ScalarMulVector Length
  | ScalarDivVector Length
  | ScalarSubVector Length
  deriving (Show, Eq)

type Dimension = (Integer, Integer)
type Length = Integer

data Assignt = Yes | No | Need deriving (Show, Eq)

data TableEntry
  = VariableEntry {depth :: Integer, nameCount :: Integer, variabbleType :: Ast.Type, assignt :: Assignt}
  | ProcedureEntry {depth :: Integer, label :: String}
  deriving (Show, Eq)

type Name = String

type NameTable = Map Name TableEntry

data CompilerState = CompilerState
  { symbolTable :: NameTable,
    depthCounter :: Integer, -- incrent when entering a block and decrement when leaving
    nameCounter :: Integer, -- initialised with 3 for each block because of DL RA and SL bevore the local variables start
    codeCounter :: Integer, -- track the current position in the instruction array
    labelCounter :: Integer
  }
  deriving (Show)

type Compiler a = State CompilerState a