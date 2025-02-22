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
  describe "testConditions" testConditions
  describe "testGenStatement" testGenStatement
  describe "testGenExpressionFromScript" testGenExpressionFromScript
  describe "genExampleProgramFromScript" genExampleProgramFromScript

testProgramm :: Spec
testProgramm = do
  describe "Machine code generation" $ do
    it "generates correct instructions for simple write program" $ do
      let programm = Ast.Program "Test" (Ast.Block [] [] [] (Ast.Write (Ast.Factor (Ast.IntLit 1))))
      let (_, finalState) = runCompilerWithState (genProgramm programm)

      -- instructions `shouldBe` [RST, LITI 1, WRI]

      depthCounter finalState `shouldBe` 0 -- Should be back to 0 after exiting block
      nameCounter finalState `shouldBe` 3 -- Should still be 3 since no variables declared
      codeCounter finalState `shouldBe` 2 -- Should be 2 since we generated 2 instructions

testGenVaribles :: Spec
testGenVaribles = do
  describe "Test single Variable declaration" $ do
    it "generates correct instructions for x float128" $ do
      let programm = ("x", Ast.NumberType (Ast.FloatType Ast.Float128))
      let (_, finalState) = runCompilerWithState (genVariable programm)

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 0
  describe "Test Variables declaration" $ do
    it "generates correct instructions for a list of variables" $ do
      let programm = [("x", Ast.NumberType $ Ast.IntType Ast.Int8), ("y", Ast.NumberType $ Ast.FloatType Ast.Float128), ("z", Ast.NumberType $ Ast.IntType Ast.Int64), ("A", Ast.VectorizedType (Ast.IntType Ast.Int8) (64, 64) Nothing), ("B", Ast.NumberType $ Ast.FloatType Ast.Float128)]
      let (instructions, finalState) = runCompilerWithState (genVariables programm)

      instructions `shouldBe` [INC 5]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 5
      codeCounter finalState `shouldBe` 1

testGenConstants :: Spec
testGenConstants = do
  describe "Test single constant declaration" $ do
    it "generates correct instructions for x float128" $ do
      let programm = ("x", Ast.NumberType $ Ast.FloatType Ast.Float128, Ast.FloatVal 99.012)
      let (instructions, finalState) = runCompilerWithState (genConstant programm)

      instructions `shouldBe` [LITF 99.012, STO 0 0]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 2
  describe "Test constants declaration" $ do
    it "generates correct instructions for list of constants" $ do
      let programm = [("x", Ast.NumberType $ Ast.IntType Ast.Int8, Ast.IntVal 200), ("y", Ast.NumberType $ Ast.FloatType Ast.Float128, Ast.FloatVal 99.012), ("z", Ast.NumberType $ Ast.IntType Ast.Int64, Ast.IntVal 999912)]
      let (instructions, finalState) = runCompilerWithState (genConstants programm)

      instructions `shouldBe` [INC 3, LITI 200, STO 0 0, LITF 99.012, STO 0 1, LITI 999912, STO 0 2]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 3
      codeCounter finalState `shouldBe` 7

testGenExpression :: Spec
testGenExpression = do
  describe "Test Unary Operation generation" $ do
    it "generates correct instructions for Neg" $ do
      let op = Ast.Neg
      let expr = Ast.Factor (Ast.Var "x")

      let initialStateWithX =
            CompilerState
              { depthCounter = 0,
                nameCounter = 1,
                codeCounter = 0,
                labelCounter = 0,
                symbolTable =
                  Map.fromList [("x", VariableEntry {depth = 0, nameCount = 0, variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64})]
              }

      let ((_, instructions), finalState) = runState (genUnary op expr) initialStateWithX

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
                labelCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ( "x",
                        VariableEntry
                          { depth = 0,
                            nameCount = 0,
                            variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64
                          }
                      )
                    ]
              }

      let ((_, instructions), finalState) = runState (genUnary op expr) initialStateWithX

      instructions `shouldBe` [LOD 0 0]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 1
  describe "Test Expression code generation" $ do
    -- Test für einfache Faktoren
    it "generates correct instructions for integer literal" $ do
      let expr = Ast.Factor (Ast.IntLit 42)
      let ((_, instructions), finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 42]
      codeCounter finalState `shouldBe` 1

    it "generates correct instructions for float literal" $ do
      let expr = Ast.Factor (Ast.FloatLit 3.14)
      let ((_, instructions), finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITF 3.14]
      codeCounter finalState `shouldBe` 1

    it "generates correct instructions for variable access" $ do
      let expr = Ast.Factor (Ast.Var "x")
      let initialStateWithX =
            CompilerState
              { depthCounter = 0,
                nameCounter = 4,
                codeCounter = 0,
                labelCounter = 0,
                symbolTable = Map.fromList [("x", VariableEntry {depth = 0, nameCount = 3, variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64})]
              }
      let ((_, instructions), finalState) = runState (genExpr expr) initialStateWithX
      instructions `shouldBe` [LOD 0 3]
      codeCounter finalState `shouldBe` 1

    it "generates correct instructions for negation" $ do
      let expr = Ast.Unary Ast.Neg (Ast.Factor (Ast.IntLit 5))
      let ((_, instructions), finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 0, LITI 5, OPR Not]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for addition" $ do
      let expr =
            Ast.Binary
              Ast.Add
              (Ast.Factor (Ast.IntLit 2))
              (Ast.Factor (Ast.IntLit 3))
      let ((_, instructions), finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 2, LITI 3, OPR Add]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for multiplication" $ do
      let expr =
            Ast.Binary
              Ast.Mul
              (Ast.Factor (Ast.IntLit 4))
              (Ast.Factor (Ast.IntLit 5))
      let ((_, instructions), finalState) = runState (genExpr expr) initialState
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
      let ((_, instructions), finalState) = runState (genExpr expr) initialState
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
      let ((_, instructions), finalState) = runState (genExpr expr) initialState
      instructions `shouldBe` [LITI 1, LITI 2, OPR Add]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for matrix multiplication" $ do
      let expr =
            Ast.Binary
              Ast.Mul
              (Ast.Factor (Ast.Var "m1"))
              (Ast.Factor (Ast.Var "m2"))
      let initialStateWithMatrix =
            CompilerState
              { depthCounter = 0,
                nameCounter = 5,
                codeCounter = 0,
                labelCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ("m1", VariableEntry {depth = 0, nameCount = 3, variabbleType = Ast.VectorizedType (Ast.IntType Ast.Int64) (10, 10) Nothing}),
                      ("m2", VariableEntry {depth = 0, nameCount = 4, variabbleType = Ast.VectorizedType (Ast.IntType Ast.Int64) (10, 10) Nothing})
                    ]
              }
      let ((_, instructions), finalState) = runState (genExpr expr) initialStateWithMatrix
      instructions `shouldBe` [LOD 0 3, LOD 0 4, OPR (MatrixMul (10, 10) (10, 10))]
      codeCounter finalState `shouldBe` 3

testConditions :: Spec
testConditions = do
  describe "Test Condition code generation" $ do
    it "generates correct instructions for equals comparison of integers" $ do
      let cond =
            Ast.Compare
              (Ast.Factor (Ast.IntLit 5))
              Ast.Eq
              (Ast.Factor (Ast.IntLit 3))
      let (instructions, finalState) = runState (genCondition cond) initialState
      instructions `shouldBe` [LITI 5, LITI 3, OPR Eq]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for less than comparison with variables" $ do
      let stateWithVars =
            initialState
              { symbolTable =
                  Map.fromList
                    [ ("x", VariableEntry 0 3 (Ast.NumberType $ Ast.IntType Ast.Int8)),
                      ("y", VariableEntry 0 4 (Ast.NumberType $ Ast.IntType Ast.Int8))
                    ]
              }
      let cond =
            Ast.Compare
              (Ast.Factor (Ast.Var "x"))
              Ast.Lt
              (Ast.Factor (Ast.Var "y"))
      let (instructions, finalState) = runState (genCondition cond) stateWithVars
      instructions `shouldBe` [LOD 0 3, LOD 0 4, OPR Lt]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for greater than comparison with float variables" $ do
      let stateWithFloats =
            initialState
              { symbolTable =
                  Map.fromList
                    [ ("x", VariableEntry 0 3 (Ast.NumberType $ Ast.FloatType Ast.Float8)),
                      ("y", VariableEntry 0 4 (Ast.NumberType $ Ast.FloatType Ast.Float8))
                    ]
              }
      let cond =
            Ast.Compare
              (Ast.Factor (Ast.Var "x"))
              Ast.Gt
              (Ast.Factor (Ast.Var "y"))
      let (instructions, finalState) = runState (genCondition cond) stateWithFloats
      instructions `shouldBe` [LOD 0 3, LOD 0 4, OPR Gt]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for not equals" $ do
      let stateWithMixed =
            initialState
              { symbolTable =
                  Map.fromList
                    [ ("x", VariableEntry 0 3 (Ast.NumberType $ Ast.IntType Ast.Int64)),
                      ("y", VariableEntry 0 4 (Ast.NumberType $ Ast.IntType Ast.Int64))
                    ]
              }
      let cond =
            Ast.Compare
              (Ast.Factor (Ast.Var "x"))
              Ast.Neq
              (Ast.Factor (Ast.IntLit 5))
      let (instructions, finalState) = runState (genCondition cond) stateWithMixed
      instructions `shouldBe` [LOD 0 3, LITI 5, OPR Not]
      codeCounter finalState `shouldBe` 3

    it "generates correct instructions for negated condition" $ do
      let cond =
            Ast.Not
              ( Ast.Compare
                  (Ast.Factor (Ast.IntLit 1))
                  Ast.Lt
                  (Ast.Factor (Ast.IntLit 2))
              )
      let (instructions, finalState) = runState (genCondition cond) initialState
      instructions `shouldBe` [LITI 1, LITI 2, OPR Lt, OPR Not]
      codeCounter finalState `shouldBe` 4

    it "generates correct instructions for nested comparison in different block" $ do
      let stateWithNestedVar =
            initialState
              { symbolTable =
                  Map.fromList
                    [ ("x", VariableEntry 1 3 (Ast.NumberType $ Ast.IntType Ast.Int64)) -- Variable from outer block
                    ],
                depthCounter = 2 -- Current block is nested
              }
      let cond =
            Ast.Compare
              (Ast.Factor (Ast.Var "x"))
              Ast.Lte
              (Ast.Factor (Ast.IntLit 10))
      let (instructions, finalState) = runState (genCondition cond) stateWithNestedVar
      instructions `shouldBe` [LOD 1 3, LITI 10, OPR Lte]
      codeCounter finalState `shouldBe` 3
      depthCounter finalState `shouldBe` 2

testGenStatement :: Spec
testGenStatement = do
  describe "Test Statement generation" $ do
    it "generates correct instructions for Assignment" $ do
      let stmt = Ast.Assignment "x" (Ast.Factor (Ast.IntLit 42))
      let initialStateWithX =
            CompilerState
              { depthCounter = 0,
                nameCounter = 1,
                codeCounter = 0,
                labelCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ( "x",
                        VariableEntry
                          { depth = 0,
                            nameCount = 0,
                            variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64
                          }
                      )
                    ]
              }

      let (instructions, finalState) = runState (genStatement stmt) initialStateWithX

      instructions `shouldBe` [LITI 42, STO 0 0]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 2

    it "generates correct instructions for Call" $ do
      let stmt = Ast.Call "proc1"
      let initialStateWithProc =
            CompilerState
              { depthCounter = 0,
                nameCounter = 1,
                codeCounter = 0,
                labelCounter = 1,
                symbolTable =
                  Map.fromList
                    [ ( "proc1",
                        ProcedureEntry
                          { depth = 0,
                            label = "procedure_proc1_1"
                          }
                      )
                    ]
              }

      let (instructions, finalState) = runState (genStatement stmt) initialStateWithProc

      instructions `shouldBe` [CAL 0 "procedure_proc1_1"]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 1

    it "generates correct instructions for Read" $ do
      let stmt = Ast.Read "x"
      let initialStateWithX =
            CompilerState
              { depthCounter = 0,
                nameCounter = 1,
                codeCounter = 0,
                labelCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ( "x",
                        VariableEntry
                          { depth = 0,
                            nameCount = 0,
                            variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64
                          }
                      )
                    ]
              }

      let (instructions, finalState) = runState (genStatement stmt) initialStateWithX

      instructions `shouldBe` [REA, STO 0 0]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 2

    it "generates correct instructions for Write" $ do
      let stmt = Ast.Write (Ast.Factor (Ast.IntLit 42))
      let (instructions, finalState) = runState (genStatement stmt) initialState

      instructions `shouldBe` [LITI 42, WRI]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 2

    it "generates correct instructions for Compound statements" $ do
      let stmt =
            Ast.Compound
              [ Ast.Assignment "x" (Ast.Factor (Ast.IntLit 1)),
                Ast.Assignment "y" (Ast.Factor (Ast.IntLit 2))
              ]
      let initialStateWithXAndY =
            CompilerState
              { depthCounter = 0,
                nameCounter = 2,
                codeCounter = 0,
                labelCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ( "x",
                        VariableEntry
                          { depth = 0,
                            nameCount = 0,
                            variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64
                          }
                      ),
                      ( "y",
                        VariableEntry
                          { depth = 0,
                            nameCount = 1,
                            variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64
                          }
                      )
                    ]
              }

      let (instructions, finalState) = runState (genStatement stmt) initialStateWithXAndY

      instructions `shouldBe` [LITI 1, STO 0 0, LITI 2, STO 0 1]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 2
      codeCounter finalState `shouldBe` 4

    it "generates correct instructions for If statement" $ do
      let condition = Ast.Compare (Ast.Factor (Ast.Var "x")) Ast.Eq (Ast.Factor (Ast.IntLit 0))
      let stmt = Ast.If condition (Ast.Assignment "y" (Ast.Factor (Ast.IntLit 1)))
      let initialStateWithXAndY =
            CompilerState
              { depthCounter = 0,
                nameCounter = 2,
                codeCounter = 0,
                labelCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ( "x",
                        VariableEntry
                          { depth = 0,
                            nameCount = 0,
                            variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64
                          }
                      ),
                      ( "y",
                        VariableEntry
                          { depth = 0,
                            nameCount = 1,
                            variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64
                          }
                      )
                    ]
              }

      let (instructions, finalState) = runState (genStatement stmt) initialStateWithXAndY

      instructions `shouldBe` [LOD 0 0, LITI 0, OPR Eq, JOF ".if_0", LITI 1, STO 0 1, LAB ".if_0"]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 2
      codeCounter finalState `shouldBe` 7

    it "generates correct instructions for While statement" $ do
      let condition = Ast.Compare (Ast.Factor (Ast.Var "i")) Ast.Lt (Ast.Factor (Ast.IntLit 10))
      let stmt = Ast.While condition (Ast.Assignment "i" (Ast.Binary Ast.Add (Ast.Factor (Ast.Var "i")) (Ast.Factor (Ast.IntLit 1))))
      let initialStateWithI =
            CompilerState
              { depthCounter = 0,
                nameCounter = 1,
                codeCounter = 0,
                labelCounter = 0,
                symbolTable =
                  Map.fromList
                    [ ( "i",
                        VariableEntry
                          { depth = 0,
                            nameCount = 0,
                            variabbleType = Ast.NumberType $ Ast.IntType Ast.Int64
                          }
                      )
                    ]
              }

      let (instructions, finalState) = runState (genStatement stmt) initialStateWithI

      instructions
        `shouldBe` [ LAB ".while_start_0",
                     LOD 0 0, -- Load i
                     LITI 10, -- Load 10
                     OPR Lt, -- Compare
                     JOF ".while_end_1", -- Jump to end if false
                     LOD 0 0, -- Load i
                     LITI 1, -- Load 1
                     OPR Add, -- Add
                     STO 0 0, -- Store in i
                     JMP ".while_start_0", -- Jump back to condition
                     LAB ".while_end_1"
                   ]
      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 1
      codeCounter finalState `shouldBe` 11

testGenExpressionFromScript :: Spec
testGenExpressionFromScript = do
  describe "Test Complex Expression generation" $ do
    it "generates correct instructions for ((3 + 5) + 2) * (12 + (9 * 2))" $ do
      -- Construct the AST for ((3 + 5) + 2) * (12 + (9 * 2))
      let leftInnerSum =
            Ast.Binary
              Ast.Add
              (Ast.Factor (Ast.IntLit 3))
              (Ast.Factor (Ast.IntLit 5))

      let leftSide =
            Ast.Binary
              Ast.Add
              leftInnerSum
              (Ast.Factor (Ast.IntLit 2))

      let rightInnerProduct =
            Ast.Binary
              Ast.Mul
              (Ast.Factor (Ast.IntLit 9))
              (Ast.Factor (Ast.IntLit 2))

      let rightSide =
            Ast.Binary
              Ast.Add
              (Ast.Factor (Ast.IntLit 12))
              rightInnerProduct

      let expr = Ast.Binary Ast.Mul leftSide rightSide

      let ((_, instructions), finalState) = runState (genExpr expr) initialState

      instructions
        `shouldBe` [ LITI 3, -- Push 3
                     LITI 5, -- Push 5
                     OPR Add, -- Add -> 8
                     LITI 2, -- Push 2
                     OPR Add, -- Add -> 10
                     LITI 12, -- Push 12
                     LITI 9, -- Push 9
                     LITI 2, -- Push 2
                     OPR Mul, -- Multiply -> 18
                     OPR Add, -- Add -> 30
                     OPR Mul -- Final multiplication -> 300
                   ]

      depthCounter finalState `shouldBe` 0
      nameCounter finalState `shouldBe` 0
      codeCounter finalState `shouldBe` 11

exampleProgramAst :: Ast.Program
exampleProgramAst =
  Ast.Program
    "Example"
    ( Ast.Block
        { Ast.constDecls = [],
          Ast.varDecls =
            [ ("a", Ast.NumberType $ Ast.IntType Ast.Int64),
              ("b", Ast.NumberType $ Ast.IntType Ast.Int64),
              ("pot", Ast.NumberType $ Ast.IntType Ast.Int64),
              ("n", Ast.NumberType $ Ast.IntType Ast.Int64),
              ("fak", Ast.NumberType $ Ast.IntType Ast.Int64)
            ],
          Ast.procedures =
            [ Ast.Procedure
                "potenz"
                ( Ast.Block
                    { Ast.constDecls = [],
                      Ast.varDecls = [("y", Ast.NumberType $ Ast.IntType Ast.Int64)],
                      Ast.procedures = [],
                      Ast.instruction =
                        Ast.Compound
                          [ Ast.Assignment "pot" (Ast.Factor (Ast.IntLit 1)),
                            Ast.Assignment "y" (Ast.Factor (Ast.Var "b")),
                            Ast.While
                              (Ast.Not (Ast.Compare (Ast.Factor (Ast.Var "y")) Ast.Lt (Ast.Factor (Ast.IntLit 1))))
                              ( Ast.Compound
                                  [ Ast.Assignment "pot" (Ast.Binary Ast.Mul (Ast.Factor (Ast.Var "pot")) (Ast.Factor (Ast.Var "a"))),
                                    Ast.Assignment "y" (Ast.Binary Ast.Sub (Ast.Factor (Ast.Var "y")) (Ast.Factor (Ast.IntLit 1)))
                                  ]
                              )
                          ]
                    }
                ),
              Ast.Procedure
                "fakultaet"
                ( Ast.Block
                    { Ast.constDecls = [],
                      Ast.varDecls = [],
                      Ast.procedures = [],
                      Ast.instruction =
                        Ast.If
                          (Ast.Compare (Ast.Factor (Ast.Var "n")) Ast.Gt (Ast.Factor (Ast.IntLit 1)))
                          ( Ast.Compound
                              [ Ast.Assignment "fak" (Ast.Binary Ast.Mul (Ast.Factor (Ast.Var "fak")) (Ast.Factor (Ast.Var "n"))),
                                Ast.Assignment "n" (Ast.Binary Ast.Sub (Ast.Factor (Ast.Var "n")) (Ast.Factor (Ast.IntLit 1))),
                                Ast.Call "fakultaet"
                              ]
                          )
                    }
                )
            ],
          Ast.instruction =
            Ast.Compound
              [ Ast.Read "a",
                Ast.Read "b",
                Ast.Call "potenz",
                Ast.Write (Ast.Factor (Ast.Var "pot")),
                Ast.Read "n",
                Ast.Assignment "fak" (Ast.Factor (Ast.IntLit 1)),
                Ast.Call "fakultaet",
                Ast.Write (Ast.Factor (Ast.Var "fak"))
              ]
        }
    )

genExampleProgramFromScript :: Spec
genExampleProgramFromScript = do
  describe "Test Programm to calculate power of 2 numbers" $ do
    it "check if the right instruction where generated" $ do
      let (instructions, _) = runState (genProgramm exampleProgramAst) initialState

      instructions
        `shouldBe` [ -- Program initialization
                     RST, -- 0: Reset machine state
                     INC 5, -- 1: Reserve space for global variables (a,b,pot,n,fak)
                     JMP ".block_5", -- 2: Jump to main program
                     LAB ".procedure_potenz_0",
                     -- PROCEDURE potenz (calculating a^b)
                     INC 1, -- 3: Reserve space for local variable y
                     LITI 1, -- 4: Load constant 1
                     STO 1 5, -- 5: pot := 1
                     LOD 1 4, -- 6: Load value of b
                     STO 0 3, -- 7: y := b
                     LAB ".while_start_1",
                     -- While condition (NOT y < 1)
                     LOD 0 3, -- 8: Load y
                     LITI 1, -- 9: Load constant 1
                     OPR Lt, -- 10: Compare less than
                     OPR Not, -- 11: Logical NOT
                     JOF ".while_end_2", -- 12: Jump to loop body if true

                     -- While loop body for potenz
                     LOD 1 5, -- 13: Load pot
                     LOD 1 3, -- 14: Load a
                     OPR Mul, -- 15: Multiply
                     STO 1 5, -- 16: pot := pot * a
                     LOD 0 3, -- 17: Load y
                     LITI 1, -- 18: Load constant 1
                     OPR Sub, -- 19: Subtract
                     STO 0 3, -- 20: y := y - 1
                     JMP ".while_start_1", -- 21: Jump to while condition
                     LAB ".while_end_2",
                     RET, -- 22

                     -- PROCEDURE fakultaet (calculating n!)
                     LAB ".procedure_fakultaet_3",
                     LOD 1 6, -- 23: Load n
                     LITI 1, -- 24: Load constant 1
                     OPR Gt, -- 25: Compare greater than
                     JOF ".if_4", -- 26: Skip if condition false (n <= 1)

                     -- Factorial calculation body
                     LOD 1 7, -- 27: Load fak
                     LOD 1 6, -- 28: Load n
                     OPR Mul, -- 29: Multiply
                     STO 1 7, -- 30: fak := fak * n
                     LOD 1 6, -- 31: Load n
                     LITI 1, -- 32: Load constant 1
                     OPR Sub, -- 33: Subtract
                     STO 1 6, -- 34: n := n - 1
                     CAL 1 ".procedure_fakultaet_3", -- 35: Recursive call to fakultaet
                     LAB ".if_4",
                     RET, -- 36: Return from procedure

                     -- Main program
                     LAB ".block_5",
                     REA, -- 37: Read input
                     STO 0 3, -- 38: READ a
                     REA, -- 39: Read input
                     STO 0 4, -- 40: READ b
                     CAL 0 ".procedure_potenz_0", -- 41: CALL potenz
                     LOD 0 5, -- 42: Load pot
                     WRI, -- 43: WRITE pot
                     REA, -- 44: Read input
                     STO 0 6, -- 45: READ n
                     LITI 1, -- 46: Load constant 1
                     STO 0 7, -- 47: fak := 1
                     CAL 0 ".procedure_fakultaet_3", -- 48: CALL fakultaet
                     LOD 0 7, -- 49: Load fak
                     WRI, -- 50: WRITE fak
                     HLT -- 51: Halt program
                   ]
