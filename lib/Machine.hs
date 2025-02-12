module Machine where

import Ast
import Control.Monad.State.Lazy
import Data.List (find)
import Debug.Trace

-- Add this helper function
debugState :: String -> Compiler ()
debugState label = do
  state <- get
  traceM $ "\n" ++ label ++ ":\n" ++ show state

data Instruction
  = RST
  | LOD Int Int
  | STO Int Int
  | INC Int
  | LIT Int
  | JMP Label
  | JOT Label
  | JOF Label
  | CAL Int Int
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

instance Show Instruction where
  show RST = "RST"
  show (LOD s i) = "LOD " ++ show s ++ " " ++ show i
  show (STO s i) = "STO " ++ show s ++ " " ++ show i
  show (INC n) = "INC " ++ show n
  show (LIT n) = "LIT " ++ show n
  show (JMP l) = "JMP " ++ show l
  show (JOT l) = "JOT " ++ show l
  show (JOF l) = "JOF " ++ show l
  show (CAL s a) = "CAL " ++ show s ++ " " ++ show a
  show RET = "RET"
  show (OPR op) = "OPR " ++ show op
  show REA = "REA"
  show WRI = "WRI"
  show HLT = "HLT"

instance Show Operator where
  show Add = "Add"
  show Sub = "Sub"
  show Mul = "Mul"
  show Div = "Div"
  show Machine.Eq = "Eq"
  show Machine.Lt = "Lt"
  show Machine.Gt = "Gt"
  show Machine.Not = "Not"

deriving instance Eq Instruction

deriving instance Eq Operator

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
  code <- genBlock block
  return $ [RST] ++ code ++ [HLT]

genBlock :: Block -> Compiler [Instruction]
genBlock (Block const variables procedures body) = do
  currentDepth <- gets depthCounter
  modify $ \s -> s {depthCounter = currentDepth + 1}
  constCode <- genConstants const
  variableCode <- genVariable variables
  proceduresCode <- genProcedures procedures
  statementCode <- genStatement body

  -- Remove symbols for this block and decrement depth counter
  -- popScope
  modify $ \s -> s {depthCounter = currentDepth}

  -- Only include JMP if there are procedures
  if null procedures
    then return $ constCode ++ variableCode ++ statementCode
    else return $ constCode ++ variableCode ++ [JMP "statement"] ++ proceduresCode ++ statementCode

genStatement :: Statement -> Compiler [Instruction]
genStatement (Assign name expression) = do
  (stufe, offset) <- lookupName name
  exprCode <- genExpr expression
  return $ exprCode ++ [STO stufe offset]
genStatement (Call name) = do
  (stufe, addr) <- lookupName name
  return [CAL stufe addr]
genStatement (Read name) = do
  (stufe, offset) <- lookupName name
  return [REA, STO stufe offset]
genStatement (Write expression) = do
  exprCode <- genExpr expression
  return $ exprCode ++ [WRI]
genStatement (Begin statements) = do
  codes <- mapM genStatement statements
  return $ concat codes
genStatement (If condition statement) = do
  labelNum <- gets codeCounter
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  let endLabel = "if_end_" ++ show labelNum
  condCode <- genCondition condition
  stmtCode <- genStatement statement
  return $
    condCode
      ++ [JOF endLabel]
      ++ stmtCode
      ++ [JMP endLabel]
genStatement (While condition statement) = do
  labelNum <- gets codeCounter
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  let startLabel = "while_" ++ show labelNum
  let endLabel = "wend_" ++ show labelNum
  condCode <- genCondition condition
  stmtCode <- genStatement statement
  return $
    [JMP startLabel]
      ++ condCode
      ++ [JOF endLabel]
      ++ stmtCode
      ++ [JMP startLabel]

genExpr :: Expression -> Compiler [Instruction]
genExpr (Num n) = return [LIT n]
genExpr (Var name) = do
  (stufe, offset) <- lookupName name
  return [LOD stufe offset]
genExpr (Plus e1 e2) = do
  code1 <- genExpr e1
  code2 <- genExpr e2
  return $ code1 ++ code2 ++ [OPR Add]
genExpr (Minus e1 e2) = do
  code1 <- genExpr e1
  code2 <- genExpr e2
  return $ code1 ++ code2 ++ [OPR Sub]
genExpr (Times e1 e2) = do
  code1 <- genExpr e1
  code2 <- genExpr e2
  return $ code1 ++ code2 ++ [OPR Mul]
genExpr (Divide e1 e2) = do
  code1 <- genExpr e1
  code2 <- genExpr e2
  return $ code1 ++ code2 ++ [OPR Div]
genExpr (Neg e) = do
  code <- genExpr e
  return $ [LIT 0] ++ code ++ [OPR Sub]

genCondition :: Condition -> Compiler [Instruction]
genCondition (Ast.Eq e1 e2) = do
  code1 <- genExpr e1
  code2 <- genExpr e2
  return $ code1 ++ code2 ++ [OPR Machine.Eq]
genCondition (Ast.Lt e1 e2) = do
  code1 <- genExpr e1
  code2 <- genExpr e2
  return $ code1 ++ code2 ++ [OPR Machine.Lt]
genCondition (Ast.Gt e1 e2) = do
  code1 <- genExpr e1
  code2 <- genExpr e2
  return $ code1 ++ code2 ++ [OPR Machine.Gt]
genCondition (Ast.Not c) = do
  code <- genCondition c
  return $ code ++ [OPR Machine.Not]

genProcedures :: [Procedure] -> Compiler [Instruction]
genProcedures [] = return []
genProcedures procs = do
  debugState "genProcedures"
  concat <$> mapM genProcedure procs

genProcedure :: Procedure -> Compiler [Instruction]
genProcedure (Procedure name block) = do
  currentAddr <- gets codeCounter
  currentDepth <- gets depthCounter
  modify $ \s -> s {nameCounter = nameCounter s + 1, symbolTable = MProcedure name currentDepth currentAddr : symbolTable s}
  debugState "before block"
  blockCode <- genBlock block
  debugState "after block"
  return $ blockCode ++ [RET]

genConstants :: [(String, Int)] -> Compiler [Instruction]
genConstants [] = return []
genConstants consts = do
  let numConsts = length consts
  let allocSpace = [INC numConsts]
  initCode <- concat <$> mapM genConst consts

  return $ allocSpace ++ initCode

genConst :: (String, Int) -> Compiler [Instruction]
genConst (name, value) = do
  depth <- gets depthCounter
  offset <- gets nameCounter
  modify $ \s -> s {nameCounter = nameCounter s + 1, symbolTable = MVariable name depth offset : symbolTable s}
  return [LIT value, STO 0 offset]

genVariable :: [String] -> Compiler [Instruction]
genVariable [] = return []
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
  return [INC $ length names]

genMVariables :: [TableEntry] -> [String] -> Int -> Int -> ([TableEntry], Int)
genMVariables acc [] depth nameCount = (acc, nameCount)
genMVariables acc (name : xs) depth nameCount = genMVariables (MVariable name depth (nameCount + 1) : acc) xs depth $ nameCount + 1

initialState :: CompilerState
initialState =
  CompilerState
    { symbolTable = [],
      depthCounter = 0,
      nameCounter = 0,
      codeCounter = 0
    }

runCompiler :: Compiler a -> a
runCompiler comp = evalState comp initialState