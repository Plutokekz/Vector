module MachineSpec where

import AbstractOpCode
import Ast qualified
import Control.Monad.State.Lazy
import Data.Map qualified as Map
import Machine
import Test.Hspec

spec :: Spec
spec = do
  -- describe "testProgramm" testProgramm
  describe "testGenVaribles" testGenVaribles
  describe "testGenConstants" testGenConstants
  describe "testGenExpression" testGenExpression

testProgramm :: Spec
testProgramm = do
  describe "Machine code generation" $ do
    it "generates correct instructions for simple write program" $ do
      let programm = Ast.Program "Test" (Ast.Block [] [] [] (Ast.Write (Ast.Factor (Ast.IntLit 1))))
      let (instructions, finalState) = runCompilerWithState (genProgramm programm)

      -- instructions `shouldBe` [RST, LITI 1, WRI]

      depthCounter finalState `shouldBe` 0 -- Should be back to 0 after exiting block
      nameCounter finalState `shouldBe` 3 -- Should still be 3 since no variables declared
      codeCounter finalState `shouldBe` 2 -- Should be 2 since we generated 2 instructions

testGenVaribles :: Spec
testGenVaribles = do
  describe "Test single Variable declaration" $ do
    it "generates correct instructions for x float128" $ do
      let programm = ("x", Ast.FloatType Ast.Float128)
      let (instructions, finalState) = runCompilerWithState (genVariable programm)

      instructions `shouldBe` [INC 1]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 1
  describe "Test Variables declaration" $ do
    it "generates correct instructions for a list of variables" $ do
      let programm = [("x", Ast.IntType Ast.Int8), ("y", Ast.FloatType Ast.Float128), ("z", Ast.IntType Ast.Int64), ("A", Ast.MatrixType (Ast.IntType Ast.Int8) (64, 64) Nothing), ("B", Ast.FloatType Ast.Float128)]
      let (instructions, finalState) = runCompilerWithState (genVariables programm)

      instructions `shouldBe` [INC 1, INC 1, INC 1, INC 1, INC 1]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 5
      codeCounter finalState `shouldBe` 5

testGenConstants :: Spec
testGenConstants = do
  describe "Test single constant declaration" $ do
    it "generates correct instructions for x float128" $ do
      let programm = ("x", Ast.FloatType Ast.Float128, Ast.FloatVal 99.012)
      let (instructions, finalState) = runCompilerWithState (genConstant programm)

      instructions `shouldBe` [INC 1, LITF 99.012, STO 0 0]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 3
  describe "Test constants declaration" $ do
    it "generates correct instructions for list of constants" $ do
      let programm = [("x", Ast.IntType Ast.Int8, Ast.IntVal 200), ("y", Ast.FloatType Ast.Float128, Ast.FloatVal 99.012), ("z", Ast.IntType Ast.Int64, Ast.IntVal 999912)]
      let (instructions, finalState) = runCompilerWithState (genConstants programm)

      instructions `shouldBe` [INC 1, LITI 200, STO 0 0, INC 1, LITF 99.012, STO 0 1, INC 1, LITI 999912, STO 0 2]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 3
      codeCounter finalState `shouldBe` 9

testGenExpression :: Spec
testGenExpression = do
  describe "Test Binary Operation generation" $ do
    it "generates correct instructions for Add" $ do
      let programm = Ast.Add
      let (instructions, finalState) = runCompilerWithState (genBinOp programm)

      instructions `shouldBe` [OPR Add]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 1
    it "generates correct instructions for Sub" $ do
      let programm = Ast.Sub
      let (instructions, finalState) = runCompilerWithState (genBinOp programm)

      instructions `shouldBe` [OPR Sub]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 1
    it "generates correct instructions for Mul" $ do
      let programm = Ast.Mul
      let (instructions, finalState) = runCompilerWithState (genBinOp programm)

      instructions `shouldBe` [OPR Mul]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 1
    it "generates correct instructions for Div" $ do
      let programm = Ast.Div
      let (instructions, finalState) = runCompilerWithState (genBinOp programm)

      instructions `shouldBe` [OPR Div]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 1
    it "generates correct instructions for MatrixMul" $ do
      let programm = Ast.MatrixMul
      let (instructions, finalState) = runCompilerWithState (genBinOp programm)

      instructions `shouldBe` [OPR MatrixMul]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 1
    it "generates correct instructions for ElementMul" $ do
      let programm = Ast.ElementMul
      let (instructions, finalState) = runCompilerWithState (genBinOp programm)

      instructions `shouldBe` [OPR ElementMul]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 1
    it "generates correct instructions for ElementDiv" $ do
      let programm = Ast.ElementDiv
      let (instructions, finalState) = runCompilerWithState (genBinOp programm)

      instructions `shouldBe` [OPR ElementDiv]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 1
  describe "Test Unary Operation generation" $ do
    it "generates correct instructions for Neg" $ do
      let op = Ast.Neg
      let expr = Ast.Factor (Ast.Var "x")

      let initialStateWithX =
            CompilerState
              { depthCounter = 0,
                nameCounter = 1,
                codeCounter = 0,
                symbolTable =
                  Map.fromList [("x", VariableEntry {depth = 0, nameCount = 0, variabbleType = Ast.IntType Ast.Int64})]
              }

      let (instructions, finalState) = runState (genUnary op expr) initialStateWithX

      instructions `shouldBe` [LITI 0, LOD 0 0, OPR Not]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 3
    it "generates correct instructions for Pos" $ do
      let op = Ast.Pos
      let expr = Ast.Factor (Ast.Var "x")

      let initialStateWithX =
            CompilerState
              { depthCounter = 0,
                nameCounter = 1,
                codeCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ( "x",
                        VariableEntry
                          { depth = 0,
                            nameCount = 0,
                            variabbleType = Ast.IntType Ast.Int64
                          }
                      )
                    ]
              }

      let (instructions, finalState) = runState (genUnary op expr) initialStateWithX

      instructions `shouldBe` [LOD 0 0]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 1
  describe "Test Expression code generation" $ do
    -- Test für einfache Faktoren
    it "generates correct instructions for integer literal" $ do
      let expr = Ast.Factor (Ast.IntLit 42)
      let (instructions, finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 42]
      codeCounter finalState `shouldBe` 1

    it "generates correct instructions for float literal" $ do
      let expr = Ast.Factor (Ast.FloatLit 3.14)
      let (instructions, finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITF 3.14]
      codeCounter finalState `shouldBe` 1

    it "generates correct instructions for variable access" $ do
      let expr = Ast.Factor (Ast.Var "x")
      let initialStateWithX =
            CompilerState
              { depthCounter = 0,
                nameCounter = 4,
                codeCounter = 0,
                symbolTable = Map.fromList [("x", VariableEntry {depth = 0, nameCount = 3, variabbleType = Ast.IntType Ast.Int64})]
              }
      let (instructions, finalState) = runState (genExpr expr) initialStateWithX
      instructions `shouldBe` [LOD 0 3]
      codeCounter finalState `shouldBe` 1

    it "generates correct instructions for negation" $ do
      let expr = Ast.Unary Ast.Neg (Ast.Factor (Ast.IntLit 5))
      let (instructions, finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 0, LITI 5, OPR Not]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for addition" $ do
      let expr =
            Ast.Binary
              Ast.Add
              (Ast.Factor (Ast.IntLit 2))
              (Ast.Factor (Ast.IntLit 3))
      let (instructions, finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 2, LITI 3, OPR Add]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for multiplication" $ do
      let expr =
            Ast.Binary
              Ast.Mul
              (Ast.Factor (Ast.IntLit 4))
              (Ast.Factor (Ast.IntLit 5))
      let (instructions, finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 4, LITI 5, OPR Mul]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for complex expression" $ do
      let expr =
            Ast.Binary
              Ast.Add
              ( Ast.Binary
                  Ast.Mul
                  (Ast.Factor (Ast.IntLit 2))
                  (Ast.Factor (Ast.IntLit 3))
              )
              (Ast.Factor (Ast.IntLit 4))
      let (instructions, finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 2, LITI 3, OPR Mul, LITI 4, OPR Add]
      codeCounter finalState `shouldBe` 5

    it "generates correct instructions for parenthesized expression" $ do
      let expr =
            Ast.Factor
              ( Ast.Parens
                  ( Ast.Binary
                      Ast.Add
                      (Ast.Factor (Ast.IntLit 1))
                      (Ast.Factor (Ast.IntLit 2))
                  )
              )
      let (instructions, finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 1, LITI 2, OPR Add]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for matrix multiplication" $ do
      let expr =
            Ast.Binary
              Ast.MatrixMul
              (Ast.Factor (Ast.Var "m1"))
              (Ast.Factor (Ast.Var "m2"))
      let initialStateWithMatrix =
            CompilerState
              { depthCounter = 0,
                nameCounter = 5,
                codeCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ("m1", VariableEntry {depth = 0, nameCount = 3, variabbleType = Ast.MatrixType (Ast.IntType Ast.Int64) (10, 10) Nothing}),
                      ("m2", VariableEntry {depth = 0, nameCount = 4, variabbleType = Ast.MatrixType (Ast.IntType Ast.Int64) (10, 10) Nothing})
                    ]
              }
      let (instructions, finalState) = runState (genExpr expr) initialStateWithMatrix
      instructions `shouldBe` [LOD 0 3, LOD 0 4, OPR MatrixMul]
      codeCounter finalState `shouldBe` 3
