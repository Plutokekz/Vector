module MachineSpec where

import Ast
import Control.Monad.State
import Machine
import Test.Hspec

spec :: Spec
spec = do
  describe "Machine code generation" $ do
    it "should compile a simple program with assignment" $ do
      let program =
            Program "Test" $
              Block
                { constants = [],
                  variables = ["x"],
                  procedures = [],
                  body = Assign "x" (Num 42)
                }

      runCompiler (genProgramm program)
        `shouldBe` [ RST,
                     INC 1, -- Allocate space for variable x
                     LIT 42, -- Push 42
                     STO 0 1, -- Store in x
                     HLT
                   ]

    it "should compile arithmetic expressions" $ do
      let program =
            Program "Test" $
              Block
                { constants = [],
                  variables = ["result"],
                  procedures = [],
                  body = Assign "result" (Plus (Num 10) (Times (Num 2) (Num 3)))
                }

      runCompiler (genProgramm program)
        `shouldBe` [ RST,
                     INC 1, -- Allocate space for result
                     LIT 10, -- Push 10
                     LIT 2, -- Push 2
                     LIT 3, -- Push 3
                     OPR Mul, -- Multiply
                     OPR Add, -- Add
                     STO 0 1, -- Store in result
                     HLT
                   ]

    it "should compile a program with if statement" $ do
      let program =
            Program "Test" $
              Block
                { constants = [],
                  variables = ["x"],
                  procedures = [],
                  body =
                    If
                      (Ast.Lt (Var "x") (Num 10))
                      (Assign "x" (Num 0))
                }

      runCompiler (genProgramm program)
        `shouldBe` [ RST,
                     INC 1, -- Allocate space for x
                     LOD 0 1, -- Load x
                     LIT 10, -- Push 10
                     OPR Machine.Lt, -- Compare
                     JOF "if_end_0", -- Jump if false
                     LIT 0, -- Push 0
                     STO 0 1, -- Store in x
                     JMP "if_end_0",
                     HLT
                   ]

    it "should compile a program with while loop" $ do
      let program =
            Program "Test" $
              Block
                { constants = [],
                  variables = ["x"],
                  procedures = [],
                  body =
                    While
                      (Ast.Gt (Var "x") (Num 0))
                      (Assign "x" (Minus (Var "x") (Num 1)))
                }

      runCompiler (genProgramm program)
        `shouldBe` [ RST,
                     INC 1, -- Allocate space for x
                     JMP "while_0",
                     LOD 0 1, -- Load x
                     LIT 0, -- Push 0
                     OPR Machine.Gt, -- Compare
                     JOF "wend_0", -- Jump if false
                     LOD 0 1, -- Load x
                     LIT 1, -- Push 1
                     OPR Sub, -- Subtract
                     STO 0 1, -- Store in x
                     JMP "while_0",
                     HLT
                   ]

    -- it "should compile a program with procedures" $ do
    --  let program =
    --        Program "Main" $
    --          Block
    --            { constants = [],
    --              variables = ["x"],
    --              procedures =
    --                [ Procedure "proc" $
    --                    Block
    --                      { constants = [],
    --                        variables = ["y"],
    --                        procedures = [],
    --                        body = Assign "y" (Num 1)
    --                      }
    --               ],
    --             body = Call "proc"
    --            }

    --  runCompiler (genProgramm program)
    --    `shouldBe` [ RST,
    --                 INC 1, -- Allocate space for x
    --                 JMP "statement",
    --                 INC 1, -- Allocate space for y in proc
    --                 LIT 1, -- Push 1
    --                 STO 0 1, -- Store in y
    --                 RET, -- Return from proc
    --                 CAL 0 3, -- Call proc
    --                 HLT
    --               ]

    it "should compile a program with READ and WRITE" $ do
      let program =
            Program "Test" $
              Block
                { constants = [],
                  variables = ["x"],
                  procedures = [],
                  body =
                    Begin
                      [ Read "x",
                        Write (Var "x")
                      ]
                }

      runCompiler (genProgramm program)
        `shouldBe` [ RST,
                     INC 1, -- Allocate space for x
                     REA, -- Read input
                     STO 0 1, -- Store in x
                     LOD 0 1, -- Load x
                     WRI, -- Write output
                     HLT
                   ]