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
  deriving (Show)

instance Show Instruction where
  show RST = "RST"
  show (LOD s i) = "LOD " ++ show s ++ " " ++ show i
  show (STO s i) = "STO " ++ show s ++ " " ++ show i
  show (INC n) = "INC " ++ show n
  show (LITI n) = "LITI " ++ show n
  show (LITF n) = "LITF " ++ show n
  show (JMP l) = "JMP " ++ show l
  show (JOT l) = "JOT " ++ show l
  show (JOF l) = "JOF " ++ show l
  show (CAL s a) = "CAL " ++ show s ++ " " ++ show a
  show RET = "RET"
  show (OPR op) = "OPR " ++ show op
  show REA = "REA"
  show WRI = "WRI"
  show HLT = "HLT"

deriving instance Eq Instruction

deriving instance Eq Operator

data TableEntry
  = MVariable {name :: String, depth :: Integer, nameCount :: Integer}
  | MProcedure {name :: String, depth :: Integer, codeAddress :: Integer}
  deriving (Show, Eq)

type NameTable = [TableEntry]

data CompilerState = CompilerState
  { symbolTable :: NameTable,
    depthCounter :: Integer,
    nameCounter :: Integer,
    codeCounter :: Integer
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

lookupName :: String -> Compiler (Integer, Integer)
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
    else return $ constCode ++ variableCode ++ [JMP 0] ++ proceduresCode ++ statementCode

genStatement :: Statement -> Compiler [Instruction]
genStatement (Assignment name expression) = do
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
genStatement (Compound statements) = do
  codes <- mapM genStatement statements
  return $ concat codes
genStatement (If condition statement) = do
  labelNum <- gets codeCounter
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  condCode <- genCondition condition
  stmtCode <- genStatement statement
  return $
    condCode
      ++ [JOF labelNum]
      ++ stmtCode
      ++ [JMP labelNum]
genStatement (While condition statement) = do
  labelNum <- gets codeCounter
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  condCode <- genCondition condition
  stmtCode <- genStatement statement
  return $
    [JMP labelNum]
      ++ condCode
      ++ [JOF labelNum]
      ++ stmtCode
      ++ [JMP labelNum]

genExpr :: Expression -> Compiler [Instruction]
genExpr (Binary op expr1 expr2) = do
  expr1Code <- genExpr expr1
  expr2Code <- genExpr expr2
  op <- genBinOp op
  return $ expr1Code ++ expr2Code ++ op
genExpr (Unary op expr) = return []
genExpr (Factor factor) = return []

genBinOp :: BinOp -> Compiler [Instruction]
genBinOp Ast.Add = return [OPR Machine.Add]
genBinOp Ast.Sub = return [OPR Machine.Sub]
genBinOp Ast.Mul = return [OPR Machine.Mul]
genBinOp Ast.Div = return [OPR Machine.Div]
genBinOp Ast.MatrixMul = return [OPR Machine.MatrixMul]
genBinOp Ast.ElementMul = return [OPR Machine.ElementMul]
genBinOp Ast.ElementDiv = return [OPR Machine.ElementDiv]

genCondition :: Condition -> Compiler [Instruction]
genCondition (Ast.Compare expr1 compOp expr2) = do
  expr1Code <- genExpr expr1
  expr2Code <- genExpr expr2
  opr <- genCompare compOp
  return $ expr1Code ++ expr2Code ++ opr
genCondition (Ast.Not condition) = do
  conditionCode <- genCondition condition
  return $ conditionCode ++ [OPR Machine.Not]

genCompare :: CompOp -> Compiler [Instruction]
genCompare Ast.Eq = do
  return [OPR Machine.Eq]
genCompare Ast.Lt = do
  return [OPR Machine.Lt]
genCompare Ast.Gt = do
  return [OPR Machine.Gt]
genCompare Ast.Lte = do
  return [OPR Machine.Lte]
genCompare Ast.Gte = do
  return [OPR Machine.Gte]
genCompare Ast.Neq = do
  return [OPR Machine.Not]

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

genConstants :: [(String, Type, Value)] -> Compiler [Instruction]
genConstants [] = return []
genConstants consts = do
  let numConsts = length consts
  let allocSpace = [INC (toInteger numConsts)]
  initCode <- concat <$> mapM genConst consts

  return $ allocSpace ++ initCode

genConst :: (String, Type, Value) -> Compiler [Instruction]
genConst (name, _, value) = do
  depth <- gets depthCounter
  offset <- gets nameCounter
  modify $ \s -> s {nameCounter = nameCounter s + 1, symbolTable = MVariable name depth offset : symbolTable s}
  case value of
    IntVal val -> return [LITI val, STO 0 offset]
    FloatVal val -> return [LITF val, STO 0 offset]

genVariable :: [(String, Type)] -> Compiler [Instruction]
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
  return [INC $ toInteger (length names)]

genMVariables :: [TableEntry] -> [(String, Type)] -> Integer -> Integer -> ([TableEntry], Integer)
genMVariables acc [] depth nameCount = (acc, nameCount)
genMVariables acc ((name, t) : xs) depth nameCount = genMVariables (MVariable name depth (nameCount + 1) : acc) xs depth $ nameCount + 1

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