module AbstractOpCode where

import Control.Monad.State.Lazy
import Data.Map (Map)
import Data.Map qualified as Map
import qualified Ast

data Instruction
  = RST
  | LOD Integer Integer
  | STO Integer Integer
  | LODN Integer Integer  -- Load n bytes from stack
  | STON Integer Integer Integer -- store n bytes on the stack
  | INC Integer
  | LITI Integer -- Load imidieate integer
  | LITF Double -- load imideate float
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
  | ElementMul
  | ElementDiv
  deriving (Show, Eq)

type Dimension = (Int, Int)

data TableEntry
  = VariableEntry {depth :: Integer, nameCount :: Integer, variabbleType :: Ast.Type}
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