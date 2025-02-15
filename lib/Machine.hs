module Machine where

import AbstractOpCode
import Ast
import Control.Monad.State
import Control.Monad.State.Lazy
import Data.List (find)
import Data.Map qualified as Map
import Debug.Trace

-- Add this helper function
debugState :: String -> Compiler ()
debugState label = do
  state <- get
  traceM $ "\n" ++ label ++ ":\n" ++ show state

pushSymbol :: TableEntry -> Name -> Compiler ()
pushSymbol entry name = modify $ \s ->
  s {symbolTable = Map.insert name entry $ symbolTable s}

popScope :: Compiler ()
popScope = do
  currentDepth <- gets depthCounter
  modify $ \s -> s {symbolTable = Map.filter (\e -> depth e /= currentDepth) (symbolTable s)}

offset :: Name -> Compiler Integer
offset name = do
  table <- gets symbolTable
  case Map.lookup name table of
    Just entry -> do
      case entry of
        VariableEntry _ count _ -> return count
        ProcedureEntry _ _ -> error $ name ++ " Is a procedure not a Variable"
    Nothing -> error $ "Name not found: " ++ name

step :: Name -> Compiler Integer
step name = do
  currentDepth <- gets depthCounter
  table <- gets symbolTable
  case Map.lookup name table of
    Just entry -> return $ currentDepth - depth entry
    Nothing -> error $ "Name not found: " ++ name

codeaddress :: Name -> Compiler Integer
codeaddress name = do
  table <- gets symbolTable
  case Map.lookup name table of
    Just entry -> do
      case entry of
        VariableEntry {} -> error $ name ++ " Is a Variable not a Procedure"
        ProcedureEntry _ addr -> return addr
    Nothing -> error $ "Name not found: " ++ name

genProgramm :: Program -> Compiler [Instruction]
genProgramm (Program name block) = do
  modify $ \s -> s {codeCounter = codeCounter s + 1} -- Add 1 to code because of rst instruction
  code <- genBlock block
  return $ [RST] ++ code ++ [HLT]

genBlock :: Block -> Compiler [Instruction]
genBlock (Block const variables procedures body) = do
  currentDepth <- gets depthCounter
  modify $ \s -> s {depthCounter = currentDepth + 1}
  constCode <- genConstants const
  variableCode <- genVariables variables
  proceduresCode <- genProcedures procedures
  statementCode <- genStatement body

  -- Remove symbols for this block and decrement depth counter
  -- popScope
  modify $ \s -> s {depthCounter = currentDepth}

  -- Only include JMP if there are procedures
  if null procedures
    then return $ constCode ++ variableCode ++ statementCode
    else return $ constCode ++ variableCode ++ [JMP 0] ++ proceduresCode ++ statementCode

genProcedures :: [Procedure] -> Compiler [Instruction]
genProcedures [] = return []
genProcedures procs = do
  debugState "genProcedures"
  concat <$> mapM genProcedure procs

genProcedure :: Procedure -> Compiler [Instruction]
genProcedure (Procedure name block) = do
  currentAddr <- gets codeCounter
  currentDepth <- gets depthCounter
  modify $ \s -> s {nameCounter = nameCounter s + 1}
  debugState "before block"
  blockCode <- genBlock block
  debugState "after block"
  return $ blockCode ++ [RET]

genStatement :: Statement -> Compiler [Instruction]
genStatement (Assignment name expression) = do
  s <- step name
  i <- offset name
  exprCode <- genExpr expression
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return $ exprCode ++ [STO s i]
genStatement (Call name) = do
  s <- step name
  a <- codeaddress name
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return [CAL s a]
genStatement (Read name) = do
  s <- step name
  i <- offset name
  modify $ \s -> s {codeCounter = codeCounter s + 2}
  return [REA, STO s i]
genStatement (Write expression) = do
  exprCode <- genExpr expression
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return $ exprCode ++ [WRI]
genStatement (Compound statements) = do
  concat <$> mapM genStatement statements
genStatement (If condition statement) = do
  condCode <- genCondition condition
  -- update code counter for jump so the offest is right
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  stmtCode <- genStatement statement
  pos <- gets codeCounter
  return $ condCode ++ [JOF pos] ++ stmtCode
genStatement (While condition statement) = do
  -- First save the position where the condition code will start
  -- We need this for the backward jump at the end
  conditionStart <- gets codeCounter
  condCode <- genCondition condition
  -- Account for the JOF instruction in the code counter
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  stmtCode <- genStatement statement
  -- Account for the JMP instruction that will loop back
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  -- Get final position for the forward jump
  endPos <- gets codeCounter
  -- Return the complete instruction sequence
  return $ condCode ++ [JOF endPos] ++ stmtCode ++ [JMP conditionStart]

genExpr :: Expression -> Compiler [Instruction]
genExpr (Binary op expr1 expr2) = do
  expr1Code <- genExpr expr1
  expr2Code <- genExpr expr2
  codeOp <- genBinOp op
  return $ expr1Code ++ expr2Code ++ codeOp
genExpr (Unary op expr) = genUnary op expr
genExpr (Factor factor) = genFactor factor

genFactor :: Factor -> Compiler [Instruction]
genFactor (Var name) = do
  s <- step name
  i <- offset name
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return [LOD s i]
genFactor (IntLit value) = do
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return [LITI value]
genFactor (FloatLit value) = do
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return [LITF value]
genFactor (Parens expr) = genExpr expr
genFactor (MatrixLit exprMatrix) = error "Not Implementen"
genFactor (MatrixIndex name (x, y)) = error "Not Implementen"

genUnary :: UnOp -> Expression -> Compiler [Instruction]
genUnary op expr = do
  case op of
    Pos -> genExpr expr
    Neg -> do
      modify $ \s -> s {codeCounter = codeCounter s + 1}
      exprCode <- genExpr expr
      modify $ \s -> s {codeCounter = codeCounter s + 1}
      -- TODO: Add right instruction for types?
      -- Expressions need a type
      return $ [LITI 0] ++ exprCode ++ [OPR AbstractOpCode.Not]

genBinOp :: BinOp -> Compiler [Instruction]
genBinOp op = do
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  genBinOp' op

genBinOp' :: BinOp -> Compiler [Instruction]
genBinOp' Ast.Add = return [OPR AbstractOpCode.Add]
genBinOp' Ast.Sub = return [OPR AbstractOpCode.Sub]
genBinOp' Ast.Mul = return [OPR AbstractOpCode.Mul]
genBinOp' Ast.Div = return [OPR AbstractOpCode.Div]
genBinOp' Ast.MatrixMul = return [OPR AbstractOpCode.MatrixMul]
genBinOp' Ast.ElementMul = return [OPR AbstractOpCode.ElementMul]
genBinOp' Ast.ElementDiv = return [OPR AbstractOpCode.ElementDiv]

genCondition :: Condition -> Compiler [Instruction]
genCondition (Ast.Compare expr1 compOp expr2) = do
  expr1Code <- genExpr expr1
  expr2Code <- genExpr expr2
  opr <- genCompare compOp
  return $ expr1Code ++ expr2Code ++ opr
genCondition (Ast.Not condition) = do
  conditionCode <- genCondition condition
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return $ conditionCode ++ [OPR AbstractOpCode.Not]

genCompare :: CompOp -> Compiler [Instruction]
genCompare op = do
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  genCompare' op

genCompare' :: CompOp -> Compiler [Instruction]
genCompare' Ast.Eq = do
  return [OPR AbstractOpCode.Eq]
genCompare' Ast.Lt = do
  return [OPR AbstractOpCode.Lt]
genCompare' Ast.Gt = do
  return [OPR AbstractOpCode.Gt]
genCompare' Ast.Lte = do
  return [OPR AbstractOpCode.Lte]
genCompare' Ast.Gte = do
  return [OPR AbstractOpCode.Gte]
genCompare' Ast.Neq = do
  return [OPR AbstractOpCode.Not]

genConstants :: [(String, Type, Value)] -> Compiler [Instruction]
genConstants consts = do
  concat <$> mapM genConstant consts

genConstant :: (String, Type, Value) -> Compiler [Instruction]
genConstant (name, constType, value) = do
  d <- gets depthCounter
  n <- gets nameCounter
  pushSymbol (VariableEntry d n constType) name
  modify $ \s -> s {nameCounter = nameCounter s + 1, codeCounter = codeCounter s + 3}
  o <- offset name
  case value of
    IntVal val -> return [INC 1, LITI val, STO 0 o]
    FloatVal val -> return [INC 1, LITF val, STO 0 o]

genVariables :: [(String, Type)] -> Compiler [Instruction]
genVariables variables = do
  concat <$> mapM genVariable variables

genVariable :: (String, Type) -> Compiler [Instruction]
genVariable (name, variableType) = do
  d <- gets depthCounter
  n <- gets nameCounter
  pushSymbol (VariableEntry d n variableType) name
  modify $ \s -> s {nameCounter = nameCounter s + 1, codeCounter = codeCounter s + 1}
  return [INC 1]

initialState :: CompilerState
initialState =
  CompilerState
    { symbolTable = Map.empty,
      depthCounter = 0,
      nameCounter = 0,
      codeCounter = 0
    }

runCompiler :: Compiler a -> a
runCompiler comp = evalState comp initialState

runCompilerWithState :: Compiler a -> (a, CompilerState)
runCompilerWithState comp = runState comp initialState