module Machine where

import AbstractOpCode
import Ast
import Control.Monad
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

getType :: Name -> Compiler Type
getType name = do
  table <- gets symbolTable
  case Map.lookup name table of
    Just entry -> do
      case entry of
        VariableEntry _ _ t -> return t
        ProcedureEntry _ _ -> error $ name ++ " Is a procedure not a Variable"
    Nothing -> error $ "Name not found: " ++ name

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

codeaddress :: Name -> Compiler String
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
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return $ [RST] ++ code ++ [HLT]

genBlock :: Block -> Compiler [Instruction]
genBlock (Block const variables procedures body) = do
  modify $ \s -> s {depthCounter = depthCounter s + 1, nameCounter = 3}
  constCode <- genConstants const
  variableCode <- genVariables variables
  instructions <-
    if not (null procedures)
      then do
        modify $ \s -> s {codeCounter = codeCounter s + 1}
        proceduresCode <- genProcedures procedures
        -- d <- gets codeCounter
        c <- gets labelCounter
        let label = ".block_" ++ show c
        modify $ \s -> s {codeCounter = codeCounter s + 1, labelCounter = labelCounter s + 1}
        statementCode <- genStatement body
        return $ constCode ++ variableCode ++ [JMP label] ++ proceduresCode ++ [LAB label] ++ statementCode
      else do
        statementCode <- genStatement body
        return $ constCode ++ variableCode ++ statementCode

  popScope
  modify $ \s -> s {depthCounter = depthCounter s - 1}
  return instructions

genProcedures :: [Procedure] -> Compiler [Instruction]
genProcedures [] = return []
genProcedures procs = do
  concat <$> mapM genProcedure procs

genProcedure :: Procedure -> Compiler [Instruction]
genProcedure (Procedure name block) = do
  d <- gets depthCounter
  -- c <- gets codeCounter
  c <- gets labelCounter
  let label = ".procedure_" ++ name ++ "_" ++ show c
  modify $ \s -> s {labelCounter = labelCounter s + 1}
  pushSymbol (ProcedureEntry d label) name
  blockCode <- genBlock block
  modify $ \s -> s {codeCounter = codeCounter s + 2}
  return $ [LAB label] ++ blockCode ++ [RET]

genStatement :: Statement -> Compiler [Instruction]
genStatement (Assignment name expression) = do
  s <- step name
  i <- offset name
  t <- getType name
  (type1, exprCode) <- genExpr expression
  when (type1 /= t) $ do
    return $ error $ "Wrong Type: " ++ show type1 ++ " != " ++ show t
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  case t of
    (VectorizedType vt dim vs) -> do
      let (w, h) = dim
      return $ exprCode ++ [STON s i (w * h)]
    _ -> return $ exprCode ++ [STO s i]
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
  (type1, exprCode) <- genExpr expression
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return $ exprCode ++ [WRI]
genStatement (Compound statements) = do
  concat <$> mapM genStatement statements
genStatement (If condition statement) = do
  condCode <- genCondition condition
  -- update code counter for jump so the offest is right
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  stmtCode <- genStatement statement
  -- pos <- gets codeCounter
  c <- gets labelCounter
  let label = ".if_" ++ show c
  modify $ \s -> s {codeCounter = codeCounter s + 1, labelCounter = labelCounter s + 1}
  return $ condCode ++ [JOF label] ++ stmtCode ++ [LAB label]
genStatement (While condition statement) = do
  -- First save the position where the condition code will start
  -- We need this for the backward jump at the end
  -- conditionStart <- gets codeCounter
  c <- gets labelCounter
  let startLabel = ".while_start_" ++ show c
  modify $ \s -> s {codeCounter = codeCounter s + 1, labelCounter = labelCounter s + 1}

  condCode <- genCondition condition
  -- Account for the JOF instruction in the code counter
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  stmtCode <- genStatement statement
  -- Account for the JMP instruction that will loop back
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  -- Get final position for the forward jump
  -- endPos <- gets codeCounter
  c <- gets labelCounter
  let endLabel = ".while_end_" ++ show c
  modify $ \s -> s {codeCounter = codeCounter s + 1, labelCounter = labelCounter s + 1}

  -- Return the complete instruction sequence
  return $ [LAB startLabel] ++ condCode ++ [JOF endLabel] ++ stmtCode ++ [JMP startLabel, LAB endLabel]

resultType :: Type -> BinOp -> Type -> Compiler (Type, Instruction)
resultType (NumberType (IntType Int64)) op (NumberType (IntType Int64)) = do
  let opInstruction = matchIntOperation op
  return (NumberType (IntType Int64), opInstruction)
-- Int64 op Vector / Matrix
resultType (NumberType (IntType Int64)) op (VectorizedType (IntType Int64) dim spec) = do
  case (op, dim) of
    (Ast.Add, (1, l)) -> return (VectorizedType (IntType Int64) (1, l) spec, OPR (ScalarAddVector l))
    (Ast.Sub, (1, l)) -> return (VectorizedType (IntType Int64) (1, l) spec, OPR (ScalarSubVector l))
    (Ast.Div, (1, l)) -> return (VectorizedType (IntType Int64) (1, l) spec, OPR (ScalarDivVector l))
    (Ast.Mul, (1, l)) -> return (VectorizedType (IntType Int64) (1, l) spec, OPR (ScalarMulVector l))
    (Ast.Add, dim) -> return (VectorizedType (IntType Int64) dim spec, OPR (ScalarAddMatrix dim))
    (Ast.Sub, dim) -> return (VectorizedType (IntType Int64) dim spec, OPR (ScalarSubMatrix dim))
    (Ast.Div, dim) -> return (VectorizedType (IntType Int64) dim spec, OPR (ScalarDivMatrix dim))
    (Ast.Mul, dim) -> return (VectorizedType (IntType Int64) dim spec, OPR (ScalarMulMatrix dim))
-- Vector / Matrix op Int64
resultType (VectorizedType (IntType Int64) dim spec) op (NumberType (IntType Int64)) = do
  case (op, dim) of
    (Ast.Add, (1, l)) -> return (VectorizedType (IntType Int64) (1, l) spec, OPR (VectorAddScalar l))
    (Ast.Sub, (1, l)) -> return (VectorizedType (IntType Int64) (1, l) spec, OPR (VectorSubScalar l))
    (Ast.Div, (1, l)) -> return (VectorizedType (IntType Int64) (1, l) spec, OPR (VectorDivScalar l))
    (Ast.Mul, (1, l)) -> return (VectorizedType (IntType Int64) (1, l) spec, OPR (VectorMulScalar l))
    (Ast.Add, dim) -> return (VectorizedType (IntType Int64) dim spec, OPR (MatrixAddScalar dim))
    (Ast.Sub, dim) -> return (VectorizedType (IntType Int64) dim spec, OPR (MatrixSubScalar dim))
    (Ast.Div, dim) -> return (VectorizedType (IntType Int64) dim spec, OPR (MatrixDivScalar dim))
    (Ast.Mul, dim) -> return (VectorizedType (IntType Int64) dim spec, OPR (MatrixMulScalar dim))
resultType (VectorizedType (IntType Int64) dim1 spec1) op (VectorizedType (IntType Int64) dim2 spec2) = do
  case op of
    Ast.Mul -> do
      let (w1, h1) = dim1
      let (w2, h2) = dim2
      -- Vector Vector mul
      if (w1 == 1) && (w2 == 1)
        then do
          when (h1 /= h1) $ do
            error $ "Connot Multiplay Vectors with diffrent lengths: " ++ show h1 ++ "!=" ++ show h2
          return (VectorizedType (IntType Int64) (w1, h2) spec1, OPR (VectorMul h1))
        else do
          -- Vector Matrix and Matrix Matrix mul
          when (h1 /= w2) $ do
            error $ "Can not multiply " ++ show dim1 ++ " by " ++ show dim2 ++ ". " ++ show h1 ++ "!=" ++ show h2
          return (VectorizedType (IntType Int64) (w1, h2) spec1, OPR (AbstractOpCode.MatrixMul dim1 dim2))
    -- Add Sub Div
    op -> do
      let (w1, h1) = dim1
      let (w2, h2) = dim2
      when ((h1 /= h2) || (w1 /= w2)) $ do
        error $ "Can not do " ++ show op ++ " when " ++ show h1 ++ "/=" ++ show h2 ++ " or " ++ show w1 ++ "/=" ++ show w2
      if h1 == 1
        then do
          let opInstruction = matchVectorOperation op h1
          return (VectorizedType (IntType Int64) (w1, h1) spec1, opInstruction)
        else do
          let opInstruction = matchMatrixOperation op dim1
          return (VectorizedType (IntType Int64) (w1, h1) spec1, opInstruction)
resultType type1 op type2 = error $ show type1 ++ show op ++ show type2 ++ " Not implemented or not supported"

matchIntOperation :: BinOp -> Instruction
matchIntOperation Ast.Add = OPR AbstractOpCode.Add
matchIntOperation Ast.Sub = OPR AbstractOpCode.Sub
matchIntOperation Ast.Div = OPR AbstractOpCode.Div
matchIntOperation Ast.Mul = OPR AbstractOpCode.Mul

matchVectorOperation :: BinOp -> Integer -> Instruction
matchVectorOperation Ast.Add l = OPR (AbstractOpCode.VectorAdd l)
matchVectorOperation Ast.Sub l = OPR (AbstractOpCode.VectorSub l)
matchVectorOperation Ast.Div l = OPR (AbstractOpCode.VectorDiv l)

matchMatrixOperation :: BinOp -> (Integer, Integer) -> Instruction
matchMatrixOperation Ast.Add d = OPR (AbstractOpCode.MatrixAdd d d)
matchMatrixOperation Ast.Sub d = OPR (AbstractOpCode.MatrixSub d d)
matchMatrixOperation Ast.Div d = OPR (AbstractOpCode.MatrixDiv d d)

genExpr :: Expression -> Compiler (Type, [Instruction])
genExpr (Binary op expr1 expr2) = do
  (type1, expr1Code) <- genExpr expr1
  (type2, expr2Code) <- genExpr expr2
  (type1, opInstruction) <- resultType type1 op type2
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return (type1, expr1Code ++ expr2Code ++ [opInstruction])
genExpr (Unary op expr) = genUnary op expr
genExpr (Factor factor) = genFactor factor

genFactor :: Factor -> Compiler (Type, [Instruction])
genFactor (Var name) = do
  s <- step name
  i <- offset name
  t <- getType name
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  case t of
    (VectorizedType vt dim vs) -> do
      let (w, h) = dim
      return (t, [LODN s i (w * h)])
    _ -> return (t, [LOD s i])
genFactor (IntLit value) = do
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return (NumberType (IntType Int64), [LIT value])
genFactor (FloatLit value) = do
  error "Not Implementen"
genFactor (Parens expr) = genExpr expr
genFactor (VectorizedLit litMatrix) = do
  let w = length litMatrix
  when (w == 0) $ do
    error "Vector Literal with length 0"
  let h = length $ head litMatrix
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  -- TODO calculate matrix or vector specifier
  return (VectorizedType (IntType Int64) (toInteger w, toInteger h) Nothing, [LITV $ concat litMatrix])
genFactor (VectorizedIndex name (Factor x, Factor y)) = do
  s <- step name
  i <- offset name
  t <- getType name
  case t of
    VectorizedType vt vd vs -> do
      case (x, y) of
        (Var x, Var y) -> do
          let indexExpr =
                Ast.Binary
                  Ast.Add
                  ( Ast.Binary
                      Ast.Mul
                      (Ast.Factor (Ast.Var y))
                      (Ast.Factor (Ast.IntLit (fst vd)))
                  )
                  (Ast.Factor (Ast.Var x))
          (type1, exprCode) <- genExpr indexExpr
          modify $ \s -> s {codeCounter = codeCounter s + 1}
          return (NumberType vt, exprCode ++ [LODO s i])
        (IntLit x, Var y) -> do
          let indexExpr =
                Ast.Binary
                  Ast.Add
                  ( Ast.Binary
                      Ast.Mul
                      (Ast.Factor (Ast.Var y))
                      (Ast.Factor (Ast.IntLit (fst vd)))
                  )
                  (Ast.Factor (Ast.IntLit x))
          (type1, exprCode) <- genExpr indexExpr
          modify $ \s -> s {codeCounter = codeCounter s + 1}
          return (NumberType vt, exprCode ++ [LODO s i])
        (Var x, IntLit y) -> do
          let indexExpr =
                Ast.Binary
                  Ast.Add
                  ( Ast.Binary
                      Ast.Mul
                      (Ast.Factor (Ast.IntLit y))
                      (Ast.Factor (Ast.IntLit (fst vd)))
                  )
                  (Ast.Factor (Ast.Var x))
          (type1, exprCode) <- genExpr indexExpr
          modify $ \s -> s {codeCounter = codeCounter s + 1}
          return (NumberType vt, exprCode ++ [LODO s i])
        (IntLit x, IntLit y) -> do
          let index = y * fst vd + x
          modify $ \s -> s {codeCounter = codeCounter s + 1}
          --  (offset + offset of element in array)
          return (NumberType vt, [LOD s (i + index)])
        _ -> error "Vector Index can only be variable or literal or combination of both"
    _ -> error "Con not index variables that is not a vector or matrix"

genUnary :: UnOp -> Expression -> Compiler (Type, [Instruction])
genUnary op expr = do
  case op of
    Pos -> genExpr expr
    Neg -> do
      modify $ \s -> s {codeCounter = codeCounter s + 1}
      (type1, exprCode) <- genExpr expr
      modify $ \s -> s {codeCounter = codeCounter s + 1}
      return (type1, [LIT 0] ++ exprCode ++ [OPR AbstractOpCode.Sub])

genCondition :: Condition -> Compiler [Instruction]
genCondition (Ast.Compare expr1 compOp expr2) = do
  (type1, expr1Code) <- genExpr expr1
  (type2, expr2Code) <- genExpr expr2
  when (type1 /= type2) $ do
    error $ "Wrong Type: " ++ show type1 ++ " != " ++ show type2
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
genConstants [] = return []
genConstants consts = do
  cons <- concat <$> mapM genConstant consts
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return $ INC (toInteger $ length consts) : cons

genConstant :: (String, Type, Value) -> Compiler [Instruction]
genConstant (name, constType, value) = do
  d <- gets depthCounter
  n <- gets nameCounter
  pushSymbol (VariableEntry d n constType) name
  modify $ \s -> s {nameCounter = nameCounter s + 1, codeCounter = codeCounter s + 2}
  o <- offset name
  case value of
    IntVal val -> return [LIT val, STO 0 o]
    FloatVal val -> error "Not Implemented"

genVariables :: [(String, Type)] -> Compiler [Instruction]
genVariables [] = return []
genVariables variables = do
  mapM_ genVariable variables
  modify $ \s -> s {codeCounter = codeCounter s + 1}
  return [INC $ toInteger $ length variables]

genVariable :: (String, Type) -> Compiler ()
genVariable (name, variableType) = do
  d <- gets depthCounter
  n <- gets nameCounter
  pushSymbol (VariableEntry d n variableType) name
  modify $ \s -> s {nameCounter = nameCounter s + 1}

initialState :: CompilerState
initialState =
  CompilerState
    { symbolTable = Map.empty,
      depthCounter = 0,
      nameCounter = 0,
      codeCounter = 0,
      labelCounter = 0
    }

runCompiler :: Compiler a -> a
runCompiler comp = evalState comp initialState

runCompilerWithState :: Compiler a -> (a, CompilerState)
runCompilerWithState comp = runState comp initialState