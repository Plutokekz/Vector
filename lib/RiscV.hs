module RiscV where

import AbstractOpCode
import Control.Monad.State
import Data.List (find)
import Data.Map qualified as Map

-- First, define a type for managing assembly generation
data AssemblyState = AssemblyState
  { labelCounter :: Integer,
    currentLabels :: Map.Map Integer String,
    opCodesPersInstruction :: [Int]
  }

type Assembly = State AssemblyState [String]

-- Main function to generate assembly
generateAssembly :: [Instruction] -> String
generateAssembly instructions =
  let initialState = AssemblyState 0 Map.empty []
      assemblyLines = evalState (generateProgram instructions) initialState
   in unlines (textSection ++ assemblyLines)

-- Generate program structure
generateProgram :: [Instruction] -> Assembly
generateProgram instructions = do
  -- Generate assembly for each instruction
  assemblies <- mapM generateAssemblyInstructions instructions
  return $ concat assemblies

-- Data section for static data
dataSection :: [String]
dataSection =
  [ ".data",
    "newline: .string \"\\n\""
  ]

-- Text section header
textSection :: [String]
textSection =
  [ ".text",
    ".global main",
    "main:"
  ]

generateAssemblyInstructions :: Instruction -> Assembly
generateAssemblyInstructions (LODO l i) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [4 + fromIntegral l]})
  return $
    [ "ld t3, 0(sp)", -- get offest from top of stack
      "li t0, 8",
      "mul t3, t3, t0", -- multiplay index times element size to get bytew offset
      "addi sp, sp, -8", -- T:=T+1
      "mv t0, fp" -- t0 := B
    ]
      ++ replicate (fromIntegral l) "ld t0, -8(t0)" -- Follow static chain depth times
      ++ [ "sub t0, t0, t3", -- ADD loaded offest to address of fp
           "ld t1, -" ++ show (i * 8 + 8) ++ "(t0)", -- Load from computed base + offset
           "sd t1, 0(sp)" -- store to stack top
         ]
generateAssemblyInstructions (LODN l i n) = do error "LODN Not Implemented"
generateAssemblyInstructions (STON l i n) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [4 + fromIntegral l]})
  return $
    -- Set vector configuration for 64-bit elements
    [ "li t0, " ++ show n,
      "vsetvli t0, t0, e64, m8, ta, ma" -- Set vector length with 64-bit element width
    ]
      ++
      -- Load from stack into vector register
      [ "add x2, sp, zero",
        "vle64.v v8, (x2)", -- Load vector from stack with 64-bit elements
        "mv t0, fp" -- t0 := B
      ]
      ++
      -- Follow static chain depth times
      replicate (fromIntegral l) "ld t0, -8(t0)"
      ++
      -- Store vector to computed base + offset
      [ "addi t0, t0, -" ++ show (i * 8 + n * 8),
        "vse64.v v8, (t0)", -- Store vector at computed base + i
        "addi sp, sp, " ++ show (n * 8) -- Adjust stack pointer
      ]
generateAssemblyInstructions (LITV elem) = do
  concat <$> mapM generateAssemblyInstructions [LIT v | v <- elem]
generateAssemblyInstructions (OPR (VectorMul l)) = do error "VectorMul Not Implemented"
generateAssemblyInstructions (OPR (VectorAdd l)) = do error "VectorAdd Not Implemented"
generateAssemblyInstructions (OPR (VectorSub l)) = do error "VectorSub Not Implemented"
generateAssemblyInstructions (OPR (VectorDiv l)) = do error "VectorDiv Not Implemented"
generateAssemblyInstructions (OPR (VectorAddScalar l)) = do error "VectorAddScalar Not Implemented"
generateAssemblyInstructions (OPR (VectorMulScalar l)) = do error "VectorMulScalar Not Implemented"
generateAssemblyInstructions (OPR (VectorDivScalar l)) = do error "VectorDivScalar Not Implemented"
generateAssemblyInstructions (OPR (VectorSubScalar l)) = do error "VectorSubScalar Not Implemented"
generateAssemblyInstructions (OPR (ScalarAddVector l)) = do error "ScalarAddVector Not Implemented"
generateAssemblyInstructions (OPR (ScalarMulVector l)) = do error "ScalarMulVector Not Implemented"
generateAssemblyInstructions (OPR (ScalarDivVector l)) = do error "ScalarDivVector Not Implemented"
generateAssemblyInstructions (OPR (ScalarSubVector l)) = do error "ScalarSubVector Not Implemented"
generateAssemblyInstructions (OPR (MatrixMul dim1 dim2)) = do error "MatrixMul dim1  Not Implemented"
generateAssemblyInstructions (OPR (MatrixAdd dim1 dim2)) = do error "MatrixAdd dim1  Not Implemented"
generateAssemblyInstructions (OPR (MatrixSub dim1 dim2)) = do error "MatrixSub dim1  Not Implemented"
generateAssemblyInstructions (OPR (MatrixDiv dim1 dim2)) = do error "MatrixDiv dim1  Not Implemented"
generateAssemblyInstructions (OPR (MatrixDivScalar dim)) = do error "MatrixDivScalar Not Implemented"
generateAssemblyInstructions (OPR (MatrixAddScalar dim)) = do error "MatrixAddScalar Not Implemented"
generateAssemblyInstructions (OPR (MatrixMulScalar dim)) = do error "MatrixMulScalar Not Implemented"
generateAssemblyInstructions (OPR (MatrixSubScalar dim)) = do error "MatrixSubScalar Not Implemented"
generateAssemblyInstructions (OPR (ScalarDivMatrix dim)) = do error "ScalarDivMatrix Not Implemented"
generateAssemblyInstructions (OPR (ScalarAddMatrix dim)) = do error "ScalarAddMatrix Not Implemented"
generateAssemblyInstructions (OPR (ScalarMulMatrix dim)) = do error "ScalarMulMatrix Not Implemented"
generateAssemblyInstructions (OPR (ScalarSubMatrix dim)) = do error "ScalarSubMatrix Not Implemented"
generateAssemblyInstructions (OPR Add) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [5]})
  return
    [ "ld t0, 0(sp)", -- t0 = store[t]
      "ld t1, 8(sp)", -- t1 = store[t-1]
      "add t3, t1, t0", -- t3 = t0 + t1
      "sd t3, 8(sp)", -- store[T-1] = t3
      "addi sp, sp, 8" -- T = T - 1
    ]
generateAssemblyInstructions (OPR Sub) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [5]})
  return
    [ "ld t0, 0(sp)", -- t0 = store[t]
      "ld t1, 8(sp)", -- t1 = store[t-1]
      "sub t3, t1, t0", -- t3 = t0 - t1
      "sd t3, 8(sp)", -- store[T-1] = t3
      "addi sp, sp, 8" -- T = T - 1
    ]
generateAssemblyInstructions (OPR Mul) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [5]})
  return
    [ "ld t0, 0(sp)", -- t0 = store[t]
      "ld t1, 8(sp)", -- t1 = store[t-1]
      "mul t3, t1, t0", -- t3 = t0 * t1
      "sd t3, 8(sp)", -- store[T-1] = t3
      "addi sp, sp, 8" -- T = T - 1
    ]
generateAssemblyInstructions (OPR Div) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [5]})
  return
    [ "ld t0, 0(sp)", -- t0 = store[t]
      "ld t1, 8(sp)", -- t1 = store[t-1]
      "div t3, t1, t0", -- t3 = t0 / t1
      "sd t3, 8(sp)", -- store[T-1] = t3
      "addi sp, sp, 8" -- T = T - 1
    ]
generateAssemblyInstructions (OPR Lt) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [5]})
  return
    [ "ld t0, 0(sp)", -- t0 = store[t]
      "ld t1, 8(sp)", -- t1 = store[t-1]
      "slt t3, t1, t0", -- Set t3 to 1 if t1 < t0, else 0
      "sd t3, 8(sp)", -- store[T-1] = t3
      "addi sp, sp, 8" -- T = T - 1
    ]
generateAssemblyInstructions (OPR Lte) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [6]})
  return
    [ "ld t0, 0(sp)", -- t0 = store[t]
      "ld t1, 8(sp)", -- t1 = store[t-1]
      "slt t3, t1, t0", -- Set t3 to 1 if t0 < t1
      "xori t3, t3, 1", -- Invert result (for <=)
      "sd t3, 8(sp)", -- store[T-1] = t3
      "addi sp, sp, 8" -- T = T - 1
    ]
generateAssemblyInstructions (OPR Gt) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [5]})
  return
    [ "ld t0, 0(sp)", -- t0 = store[t]
      "ld t1, 8(sp)", -- t1 = store[t-1]
      "slt t3, t0, t1", -- Set t3 to 1 if t0 < t1
      "sd t3, 8(sp)", -- store[T-1] = t3
      "addi sp, sp, 8" -- T = T - 1
    ]
generateAssemblyInstructions (OPR Gte) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [6]})
  return
    [ "ld t0, 0(sp)", -- t0 = store[t]
      "ld t1, 8(sp)", -- t1 = store[t-1]
      "slt t3, t0, t1", -- Set t3 to 1 if t1 < t0
      "xori t3, t3, 1", -- Invert result (for >=)
      "sd t3, 8(sp)", -- store[T-1] = t3
      "addi sp, sp, 8" -- T = T - 1
    ]
generateAssemblyInstructions (OPR Not) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [3]})
  return
    [ "ld t0, 0(sp)", -- Load top value
      "andi t0, t0, 1", -- Mask the loaded value to keep only the least significant bit
      "xori t0, t0, 1", --   # XOR with 1 to invert the bit
      "sd t0, 0(sp)" -- Store result (no stack adjustment needed)
    ]
generateAssemblyInstructions (LIT value) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [3]})
  return
    [ "addi sp, sp, -8", -- T = T + 1
      "li t0, " ++ show value,
      "sd t0, 0(sp)"
    ]
generateAssemblyInstructions (LOD s i) = generateLODAssembly s i
generateAssemblyInstructions RST = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [5]})
  return
    [ "mv fp, sp", -- B := T Because we dont know the addres of the current stack
      "addi sp, sp, -24", -- T:=B+3 make space for dl ra and sl (8 bytes each)
      "li t0, 1", -- DEBUG
      "sd t0, 16(sp)", -- DL = 0 (8 bytes)
      "sd t0, 8(sp)", -- RA = 0 (8 bytes)
      "sd t0, 0(sp)" -- SL = 0 (8 bytes)
    ]
generateAssemblyInstructions (STO s i) = generateSTOREAssembly s i
generateAssemblyInstructions (INC i) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [1]})
  return ["addi sp, sp, -" ++ show (i * 8)]
generateAssemblyInstructions (JMP a) = do
  return ["j " ++ a]
generateAssemblyInstructions (JOT a) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [3]})
  return
    [ "ld t0, 0(sp)", -- t0 := store[T]
      "addi sp, sp, 8", -- decrease stack ??
      "li t1, 1", -- t1 := 1
      "beq t0, t1, " ++ a
    ]
generateAssemblyInstructions (JOF a) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [2]})
  return
    [ "ld t0, 0(sp)", -- t0 := store[T]
      "addi sp, sp, 8", -- decrease stack ??
      "beq t0, zero, " ++ a
    ]
generateAssemblyInstructions (CAL stepp address) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [8 + fromInteger stepp]})
  return $
    [ "addi sp, sp, -24", -- T := T+1 (make space for descriptor, 24 bytes total)
      "sd fp, 16(sp)", -- store[T+0] := B (store DL)
      "auipc ra, 0",
      "addi ra, ra, " ++ show (stepp * 4 + 8 * 4),
      "sd ra, 8(sp)", -- store[T+1] := P (store RA)
      "mv t0, fp" -- prepare to follow static chain
    ]
      ++
      -- Follow static chain s times to find static predecessor
      replicate (fromIntegral stepp) "ld t0, -8(t0)"
      ++ [ "sd t0, 0(sp)", -- store[T+2] := base(s) (store SL)
           "mv fp, sp", -- B := T (set new base)
           "addi fp, fp, 24", -- adjust so we at the beggining of the segment not after the Descriptor segment
           -- "addi sp, sp, 16", -- T := B+2 (adjust stack top)
           "jal zero, " ++ address -- P := a (jump to procedure)
         ]
generateAssemblyInstructions RET = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [1]})
  return
    [ "ld ra, -16(fp)", -- load return address (P := store[B+1])
      "mv sp, fp", -- set stack pointer to base (preparation for T := B-1)
      "ld fp, -8(fp)", -- restore old base (B := store[B])
      -- "addi sp, sp, 8", -- T := B-1 (adjust stack top)
      "jalr zero, 0(ra)" -- jump to return address
    ]
generateAssemblyInstructions REA = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [4]})
  return
    [ "addi sp, sp, -8", -- T := T+1 (make space on stack)
      "li a7, 5", -- load syscall number for read integer
      "ecall", -- system call
      "sd a0, 0(sp)" -- store[T] := read() (save input to stack)
    ]
generateAssemblyInstructions WRI = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [7]})
  return
    [ "ld a0, 0(sp)", -- load value from stack top
      "li a7, 1", -- load syscall number for print integer
      "ecall", -- system call
      "li a7, 11", -- load syscall number for print character
      "li a0, 10", -- load newline character
      "ecall", -- print newline
      "addi sp, sp, 8" -- T := T-1 (adjust stack pointer)
    ]
generateAssemblyInstructions HLT = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [2]})
  return
    [ "li a0, 0", -- 0 signals success
      "li a7, 93", -- load syscall number for exit
      "ecall" -- terminate program
    ]
generateAssemblyInstructions (LAB label) = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [1]})
  return [label ++ ":"]

generateLODAssembly l i = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [4 + fromIntegral l]})
  return $
    [ "addi sp, sp, -8", -- T:=T+1
      "mv t0, fp" -- t0 := B
    ]
      ++ replicate (fromIntegral l) "ld t0, -8(t0)" -- Follow static chain depth times
      ++ [ "ld t1, -" ++ show (i * 8 + 8) ++ "(t0)", -- Load from computed base + offset
           "sd t1, 0(sp)" -- store to stack top
         ]

generateSTOREAssembly l i = do
  modify (\s -> s {opCodesPersInstruction = opCodesPersInstruction s ++ [4 + fromIntegral l]})
  return $
    [ "ld t1, 0(sp)", -- t1 := store[T]
      "mv t0, fp" -- t0 := B
    ]
      ++
      -- Follow static chain depth times
      replicate (fromIntegral l) "ld t0, -8(t0)"
      ++
      -- Store to computed base + offset
      [ "sd t1, -" ++ show (i * 8 + 8) ++ "(t0)", -- store at computed base + i
        "addi sp, sp, 8" -- T := T-1
      ]
