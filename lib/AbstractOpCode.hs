module AbstractOpCode where

import Control.Monad.State.Lazy
import Data.Map (Map)
import Data.Map qualified as Map
import qualified Ast

data Instruction
  = RST
  | LOD Integer Integer
  | STO Integer Integer
  | INC Integer
  | LITI Integer -- Load imidieate integer
  | LITF Double -- load imideate float
  | JMP Integer
  | JOT Integer
  | JOF Integer
  | CAL Integer Integer
  | RET
  | OPR Operator
  | REA
  | WRI
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
  | MatrixMul
  | ElementMul
  | ElementDiv
  deriving (Show, Eq)

data TableEntry
  = VariableEntry {depth :: Integer, nameCount :: Integer, variabbleType :: Ast.Type}
  | ProcedureEntry {depth :: Integer, codeAddress :: Integer}
  deriving (Show, Eq)

type Name = String

type NameTable = Map Name TableEntry

data CompilerState = CompilerState
  { symbolTable :: NameTable,
    depthCounter :: Integer, -- incrent when entering a block and decrement when leaving
    nameCounter :: Integer, -- initialised with 3 for each block because of DL RA and SL bevore the local variables start
    codeCounter :: Integer -- track the current position in the instruction array
  }
  deriving (Show)

type Compiler a = State CompilerState a