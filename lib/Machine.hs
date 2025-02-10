module Machine where

import Ast
import Control.Monad.State (State, gets, modify)
import Data.List (find)

data Instruction
  = RST
  | LOD Int Int
  | STO Int Int
  | INC Int
  | LIT Int
  | JMP Label
  | JOT Label
  | JOF Label
  | CAL Int Label
  | RET
  | OPR Operator
  | REA
  | WRI
  | HLT

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Gt
  | Not

type Label = String

data TableEntry
  = MVariable {name :: String, depth :: Int, nameCount :: Int}
  | MProcedure {name :: String, depth :: Int, codeAddress :: Int}
  deriving (Show, Eq)

type NameTable = [TableEntry]

data CompilerState = CompilerState
  { symbolTable :: NameTable,
    depthCounter :: Int,
    nameCounter :: Int,
    codeCounter :: Int
  }
  deriving (Show)

type Compiler a = State CompilerState a

pushSymbol :: TableEntry -> Compiler ()
pushSymbol entry = modify $ \s ->
  s {symbolTable = entry : symbolTable s}

popScope :: Compiler ()
popScope = do
  currentDepth <- gets depthCounter
  modify $ \s -> s {symbolTable = filter (\e -> depth e /= currentDepth) (symbolTable s)}

lookupName :: String -> Compiler (Int, Int)
lookupName findName = do
  currentDepth <- gets depthCounter
  table <- gets symbolTable
  case find (\e -> findName == name e) table of
    Just entry -> do
      let stufe = currentDepth - depth entry
      let addr = case entry of
            MVariable _ _ off -> off
            MProcedure _ _ code -> code
      return (stufe, addr)
    Nothing -> error $ "Name not found: " ++ findName

genProgramm :: Program -> Compiler [Instruction]
genProgramm (Program name block) = do
  modify $ \s -> s {depthCounter = 1, nameCounter = 1, symbolTable = MProcedure name 0 0 : symbolTable s}
  code <- genBlock block
  return $ [RST] ++ code ++ [HLT]

genBlock :: Block -> Compiler [Instruction]
genBlock (Block const variables procedures body) = do
  modify $ \s -> s {depthCounter = 1}
  constCode <- genConst const
  variableCode <- genVariable variables
  proceduresCode <- genProcedure procedures
  statementCode <- genStatement body
  return $ constCode ++ variableCode ++ [JMP "statement"] ++ proceduresCode ++ statementCode

genStatement :: Statement -> Compiler [Instruction]
genStatement _ = return []

genProcedure :: [Procedure] -> Compiler [Instruction]
genProcedure _ = return []

genConst :: [(String, Int)] -> Compiler [Instruction]
genConst _ = return []

genVariable :: [String] -> Compiler [Instruction]
genVariable names = do
  nameCounter <- gets nameCounter
  currentDepth <- gets depthCounter
  let (entries, nameCount) = genMVariables [] names currentDepth nameCounter
  modify $ \s ->
    s
      { depthCounter = 1,
        nameCounter = nameCount,
        symbolTable = entries ++ symbolTable s
      }
  return [INC counter | (MVariable name depth counter) <- entries]

genMVariables :: [TableEntry] -> [String] -> Int -> Int -> ([TableEntry], Int)
genMVariables acc [] depth nameCount = (acc, nameCount)
genMVariables acc (name : xs) depth nameCount = genMVariables (MVariable name depth (nameCount + 1) : acc) xs depth $ nameCount + 1