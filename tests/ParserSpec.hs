module ParserSpec (spec) where

import Ast
import Control.Monad.Except
import Control.Monad.State
import Lexer (tokenize)
import SimpleParser
import Test.Hspec

-- Helper functions to test individual parser components
testParser :: Parser a -> String -> Either String a
testParser parser input =
  case tokenize input of
    Left err -> Left (show err)
    Right (_, tokens, _) ->
      case evalState (runExceptT parser) (initParserState tokens) of
        Left err -> Left (showParseError input err)
        Right ast -> Right ast

spec :: Spec
spec = do
  describe "testParseProgram" testParseProgram
  describe "testParseBlock" testParseBlock
  describe "testParseConstDecls" testParseConstDecls
  describe "testParseVarDecls" testParseVarDecls
  describe "testParseType" testParseType
  describe "testParseProcDecls" testParseProcDecls
  describe "testParseStatement" testParseStatement
  describe "testParseExpression" testParseExpression
  describe "testParseCondition" testParseCondition
  describe "testMatrixVectorOperations" testMatrixVectorOperations
  describe "testMatrixGenerators" testMatrixGenerators

testParseProgram :: Spec
testParseProgram = do
  describe "SimpleParser.parseProgram" $ do
    it "parses a minimal valid program" $ do
      let input = "PROGRAM Test: BEGIN x = 1 END."
      let result = parse input
      result `shouldBe` Right (Program "Test" (Block [] [] [] (Compound [Assignment "x" (Factor (IntLit 1))])))

    it "fails on missing PROGRAM keyword" $ do
      let input = "Test: BEGIN x = 1 END."
      let result = parse input
      result `shouldBe` Left "Parse error at position 0: Expected one from [PROGRAM], got Identifier \"Test\"\nNear: <ERROR LOCATION>\n"

    it "fails on missing program name" $ do
      let input = "PROGRAM : BEGIN x = 1 END."
      let result = parse input
      result `shouldBe` Left "Parse error at position 8: Expected one from [Identifier \"\"], got Colon\nNear: PROGRAM <ERROR LOCATION>\n"

testParseBlock :: Spec
testParseBlock = do
  describe "SimpleParser.parseBlock" $ do
    it "parses minimal block" $ do
      let input = "BEGIN x = 1 END"
      let result = testParser parseBlock input
      result `shouldBe` Right (Block [] [] [] (Compound [Assignment "x" (Factor (IntLit 1))]))

    it "parses block with variable declarations" $ do
      let input = "VAR INT32 x; BEGIN x = 1 END"
      let result = testParser parseBlock input
      result `shouldBe` Right (Block [] [("x", NumberType (IntType Int32))] [] (Compound [Assignment "x" (Factor (IntLit 1))]))

    it "parses block with multiple instructions" $ do
      let input = "BEGIN x = 1; y = 2; z = 3 END"
      let result = testParser parseBlock input
      result
        `shouldBe` Right
          ( Block
              []
              []
              []
              ( Compound
                  [ Assignment "x" (Factor (IntLit 1)),
                    Assignment "y" (Factor (IntLit 2)),
                    Assignment "z" (Factor (IntLit 3))
                  ]
              )
          )

    it "parses block with matrix assignments using generators" $ do
      let input =
            unlines
              [ "VAR INT32 DIM(2, 2) mA;",
                "VAR FLOAT32 DIM(3, 3) mB;",
                "BEGIN",
                "  mA = GenId DIM(2, 2);",
                "  mB = GenRandom DIM(3, 3) FLOAT32",
                "END"
              ]
      let result = testParser parseBlock input
      case result of
        Right (Block [] vars [] (Compound stmts)) -> do
          -- Check variable declarations
          vars
            `shouldBe` [ ("mA", VectorizedType (IntType Int32) (2, 2) Nothing),
                         ("mB", VectorizedType (FloatType Float32) (3, 3) Nothing)
                       ]
          -- Check assignments
          length stmts `shouldBe` 2
          case head stmts of
            Assignment "mA" expr -> do
              putStrLn $ "Got expression for mA: " ++ show expr
              expr `shouldBe` Factor (VectorizedLit [[1, 0], [0, 1]])
            other -> expectationFailure $ "Expected Assignment but got: " ++ show other
          case stmts !! 1 of
            Assignment "mB" _ -> return () -- Random values, can't check exact values
            other -> expectationFailure $ "Expected Assignment but got: " ++ show other
        Right other -> expectationFailure $ "Got unexpected block structure: " ++ show other
        Left err -> expectationFailure $ "Parser failed: " ++ err

testParseConstDecls :: Spec
testParseConstDecls = do
  describe "SimpleParser.parseConstDecls" $ do
    it "parses single constant declaration" $ do
      let input = "CONST INT32 x = 42;"
      let result = testParser parseConstDecls input
      result `shouldBe` Right [("x", NumberType (IntType Int32), IntVal 42)]

    it "parses multiple constant declarations" $ do
      let input = "CONST INT32 x = 42, y = 24;"
      let result = testParser parseConstDecls input
      result
        `shouldBe` Right
          [ ("x", NumberType (IntType Int32), IntVal 42),
            ("y", NumberType (IntType Int32), IntVal 24)
          ]

    it "parses vector constant declaration" $ do
      let input = "CONST INT32 DIM(3) v = [1, 2, 3];"
      let result = testParser parseConstDecls input
      result
        `shouldBe` Right
          [ ( "v",
              VectorizedType (IntType Int32) (1, 3) Nothing,
              MatrixVal [[IntVal 1, IntVal 2, IntVal 3]]
            )
          ]

    it "parses matrix constant declaration" $ do
      let input = "CONST INT32 DIM(2, 2) m = [[1, 2], [3, 4]];"
      let result = testParser parseConstDecls input
      result
        `shouldBe` Right
          [ ( "m",
              VectorizedType (IntType Int32) (2, 2) Nothing,
              MatrixVal [[IntVal 1, IntVal 2], [IntVal 3, IntVal 4]]
            )
          ]

    it "parses identity matrix constant declaration" $ do
      let input = "CONST INT32 DIM(2, 2) Identity I = [[1, 0], [0, 1]];"
      let result = testParser parseConstDecls input
      result
        `shouldBe` Right
          [ ( "I",
              VectorizedType (IntType Int32) (2, 2) (Just Ast.Identity),
              MatrixVal [[IntVal 1, IntVal 0], [IntVal 0, IntVal 1]]
            )
          ]

    it "parses multiple CONST declarations with different types" $ do
      let input =
            unlines
              [ "CONST INT32 x = 42;",
                "CONST INT32 DIM(3) v = [1, 2, 3];",
                "CONST INT32 DIM(2, 2) A = [[1, 2], [3, 4]];"
              ]
      let result = testParser parseConstDecls input
      result
        `shouldBe` Right
          [ ("x", NumberType (IntType Int32), IntVal 42),
            ("v", VectorizedType (IntType Int32) (1, 3) Nothing, MatrixVal [[IntVal 1, IntVal 2, IntVal 3]]),
            ("A", VectorizedType (IntType Int32) (2, 2) Nothing, MatrixVal [[IntVal 1, IntVal 2], [IntVal 3, IntVal 4]])
          ]

    it "parses complex declarations with generators" $ do
      let input =
            unlines
              [ "CONST INT32 DIM(3, 3) Identity mA = GenId DIM(3, 3);",
                "CONST FLOAT32 DIM(2, 4) mB = GenFromVal 3.14 DIM(2, 4);",
                "CONST INT8 DIM(2, 2) mC = GenRandom DIM(2, 2) INT8;"
              ]
      let result = testParser parseConstDecls input
      case result of
        Right consts -> do
          length consts `shouldBe` 3
          -- Check first constant (identity matrix)
          let (name1, type1, val1) = head consts
          name1 `shouldBe` "mA"
          type1 `shouldBe` VectorizedType (IntType Int32) (3, 3) (Just Ast.Identity)
          case val1 of
            MatrixVal m -> do
              length m `shouldBe` 3
              all (\row -> length row == 3) m `shouldBe` True
              and
                [ case m !! i !! j of
                    IntVal v -> v == if i == j then 1 else 0
                    _ -> False
                  | i <- [0 .. 2],
                    j <- [0 .. 2]
                ]
                `shouldBe` True
            _ -> expectationFailure "Expected MatrixVal for mA"

          -- Check second constant (from value)
          let (name2, type2, val2) = consts !! 1
          name2 `shouldBe` "mB"
          type2 `shouldBe` VectorizedType (FloatType Float32) (2, 4) Nothing
          case val2 of
            MatrixVal m -> do
              length m `shouldBe` 2
              all (\row -> length row == 4) m `shouldBe` True
              all (all (== FloatVal 3.14)) m `shouldBe` True
            _ -> expectationFailure "Expected MatrixVal for mB"

          -- Check third constant (random)
          let (name3, type3, val3) = consts !! 2
          name3 `shouldBe` "mC"
          type3 `shouldBe` VectorizedType (IntType Int8) (2, 2) Nothing
          case val3 of
            MatrixVal m -> do
              length m `shouldBe` 2
              all (\row -> length row == 2) m `shouldBe` True
              all (all isValidInt8) m `shouldBe` True
            _ -> expectationFailure "Expected MatrixVal for mC"
        Left err -> expectationFailure $ "Failed to parse: " ++ err

testParseVarDecls :: Spec
testParseVarDecls = do
  describe "SimpleParser.parseVarDecls" $ do
    it "parses single variable declaration" $ do
      let input = "VAR INT32 x;"
      let result = testParser parseVarDecls input
      result `shouldBe` Right [("x", NumberType (IntType Int32))]

    it "parses multiple variable declarations" $ do
      let input = "VAR INT32 x; FLOAT64 y;"
      let result = testParser parseVarDecls input
      result
        `shouldBe` Right
          [ ("x", NumberType (IntType Int32)),
            ("y", NumberType (FloatType Float64))
          ]

    it "parses declarations with matrix types" $ do
      let input =
            unlines
              [ "VAR INT32 DIM(3, 3) Identity mA;",
                "VAR FLOAT32 DIM(2, 4) mB;",
                "VAR INT8 DIM(2, 2) mC;"
              ]
      let result = testParser parseVarDecls input
      result
        `shouldBe` Right
          [ ("mA", VectorizedType (IntType Int32) (3, 3) (Just Ast.Identity)),
            ("mB", VectorizedType (FloatType Float32) (2, 4) Nothing),
            ("mC", VectorizedType (IntType Int8) (2, 2) Nothing)
          ]

testParseType :: Spec
testParseType = do
  describe "SimpleParser.parseType" $ do
    it "parses simple number types" $ do
      let input = "INT32"
      let result = testParser parseType input
      result `shouldBe` Right (NumberType (IntType Int32))

    it "parses vector types" $ do
      let input = "INT32 DIM(10, 20)"
      let result = testParser parseType input
      result `shouldBe` Right (VectorizedType (IntType Int32) (10, 20) Nothing)

    it "parses vector types with specifier" $ do
      let input = "INT32 DIM(10, 10) Identity"
      let result = testParser parseType input
      result `shouldBe` Right (VectorizedType (IntType Int32) (10, 10) (Just Ast.Identity))

testParseProcDecls :: Spec
testParseProcDecls = do
  describe "SimpleParser.parseProcDecls" $ do
    it "parses empty procedure list" $ do
      let input = "BEGIN x = 1 END"
      let result = testParser parseProcDecls input
      result `shouldBe` Right []

    it "parses single procedure declaration" $ do
      let input = "PROCEDURE procA; BEGIN x = 1 END;"
      let result = testParser parseProcDecls input
      result `shouldBe` Right [Procedure "procA" (Block [] [] [] (Compound [Assignment "x" (Factor (IntLit 1))]))]

testParseStatement :: Spec
testParseStatement = do
  describe "SimpleParser.parseStatement" $ do
    it "parses assignment statement" $ do
      let input = "x = 42"
      let result = testParser parseInstruction input
      result `shouldBe` Right (Assignment "x" (Factor (IntLit 42)))

    it "parses call statement" $ do
      let input = "CALL procA"
      let result = testParser parseInstruction input
      result `shouldBe` Right (Call "procA")

    it "parses read statement" $ do
      let input = "READ x"
      let result = testParser parseInstruction input
      result `shouldBe` Right (Read "x")

    it "parses write statement" $ do
      let input = "WRITE 42"
      let result = testParser parseInstruction input
      result `shouldBe` Right (Write (Factor (IntLit 42)))

    it "parses if statement" $ do
      let input = "IF x == 0 THEN BEGIN x = 1 END"
      let result = testParser parseInstruction input
      result `shouldBe` Right (If (Compare (Factor (Var "x")) Eq (Factor (IntLit 0))) (Compound [Assignment "x" (Factor (IntLit 1))]))

    it "parses while statement" $ do
      let input = "WHILE x > 0 DO BEGIN x = x - 1 END"
      let result = testParser parseInstruction input
      result
        `shouldBe` Right
          ( While
              (Compare (Factor (Var "x")) Gt (Factor (IntLit 0)))
              (Compound [Assignment "x" (Binary Sub (Factor (Var "x")) (Factor (IntLit 1)))])
          )

    it "parses standalone expression evaluation" $ do
      let input = "5 + 3 + 7 > 10"
      let result = testParser parseCondition input
      result
        `shouldBe` Right
          ( Compare
              ( Binary
                  Add
                  ( Binary
                      Add
                      (Factor (IntLit 5))
                      (Factor (IntLit 3))
                  )
                  (Factor (IntLit 7))
              )
              Gt
              (Factor (IntLit 10))
          )

testParseExpression :: Spec
testParseExpression = do
  describe "SimpleParser.parseExpression" $ do
    it "parses simple number" $ do
      let input = "42"
      let result = testParser parseExpression input
      result `shouldBe` Right (Factor (IntLit 42))

    it "parses addition" $ do
      let input = "1 + 2"
      let result = testParser parseExpression input
      result `shouldBe` Right (Binary Add (Factor (IntLit 1)) (Factor (IntLit 2)))

    it "parses multiplication" $ do
      let input = "2 * 3"
      let result = testParser parseExpression input
      result `shouldBe` Right (Binary Mul (Factor (IntLit 2)) (Factor (IntLit 3)))

    it "parses complex expression with parentheses" $ do
      let input = "(1 + 2) * 3"
      let result = testParser parseExpression input
      result `shouldBe` Right (Binary Mul (Factor (Parens (Binary Add (Factor (IntLit 1)) (Factor (IntLit 2))))) (Factor (IntLit 3)))

    it "parses vector literal" $ do
      let input = "[3]"
      let result = testParser parseExpression input
      result `shouldBe` Right (Factor (VectorizedLit [[3]]))

    it "parses multi-element vector literal" $ do
      let input = "[1, 2, 3]"
      let result = testParser parseExpression input
      result `shouldBe` Right (Factor (VectorizedLit [[1, 2, 3]]))

    it "parses matrix literal" $ do
      let input = "[[1, 2], [3, 4]]"
      let result = testParser parseExpression input
      result `shouldBe` Right (Factor (VectorizedLit [[1, 2], [3, 4]]))

testParseCondition :: Spec
testParseCondition = do
  describe "SimpleParser.parseCondition" $ do
    it "parses simple equality condition" $ do
      let input = "x == 0"
      let result = testParser parseCondition input
      result `shouldBe` Right (Compare (Factor (Var "x")) Eq (Factor (IntLit 0)))

    it "parses negated condition" $ do
      let input = "NOT x == 0"
      let result = testParser parseCondition input
      result `shouldBe` Right (Not (Compare (Factor (Var "x")) Eq (Factor (IntLit 0))))

    it "parses less than condition" $ do
      let input = "x < 10"
      let result = testParser parseCondition input
      result `shouldBe` Right (Compare (Factor (Var "x")) Lt (Factor (IntLit 10)))

    it "parses greater than or equal condition" $ do
      let input = "x >= 5"
      let result = testParser parseCondition input
      result `shouldBe` Right (Compare (Factor (Var "x")) Gte (Factor (IntLit 5)))

testMatrixVectorOperations :: Spec
testMatrixVectorOperations = do
  describe "Matrix and Vector Operations" $ do
    it "parses matrix multiplication" $ do
      let input = "A * B"
      let result = testParser parseExpression input
      result `shouldBe` Right (Binary Mul (Factor (Var "A")) (Factor (Var "B")))

    it "parses element-wise matrix multiplication" $ do
      let input = "A .* B"
      let result = testParser parseExpression input
      result `shouldBe` Right (Binary Ast.ElementMul (Factor (Var "A")) (Factor (Var "B")))

testMatrixGenerators :: Spec
testMatrixGenerators = do
  describe "SimpleParser.parseMatrixGenerator" $ do
    it "parses GenFromVal for vector" $ do
      let input = "GenFromVal 42 DIM(3)"
      let result = testParser parseMatrixGenerator input
      result `shouldBe` Right (MatrixVal [[IntVal 42, IntVal 42, IntVal 42]])

    it "parses GenFromVal for matrix" $ do
      let input = "GenFromVal 42 DIM(2, 3)"
      let result = testParser parseMatrixGenerator input
      result `shouldBe` Right (MatrixVal [[IntVal 42, IntVal 42, IntVal 42], [IntVal 42, IntVal 42, IntVal 42]])

    it "parses GenFromVal with float value" $ do
      let input = "GenFromVal 3.14 DIM(2, 2)"
      let result = testParser parseMatrixGenerator input
      result `shouldBe` Right (MatrixVal [[FloatVal 3.14, FloatVal 3.14], [FloatVal 3.14, FloatVal 3.14]])

    it "parses GenId for vector" $ do
      let input = "GenId DIM(3)"
      let result = testParser parseMatrixGenerator input
      result `shouldBe` Right (MatrixVal [[IntVal 1, IntVal 0, IntVal 0]])

    it "parses GenId for matrix" $ do
      let input = "GenId DIM(3, 3)"
      let result = testParser parseMatrixGenerator input
      result
        `shouldBe` Right
          ( MatrixVal
              [ [IntVal 1, IntVal 0, IntVal 0],
                [IntVal 0, IntVal 1, IntVal 0],
                [IntVal 0, IntVal 0, IntVal 1]
              ]
          )

    it "parses GenRandom for INT8" $ do
      let input = "GenRandom DIM(2, 2) INT8"
      let result = testParser parseMatrixGenerator input
      case result of
        Right (MatrixVal matrix) -> do
          length matrix `shouldBe` 2
          all (\row -> length row == 2) matrix `shouldBe` True
          all (all isValidInt8) matrix `shouldBe` True
        _ -> expectationFailure "Expected a MatrixVal with 2x2 INT8 values"

-- Removed for now as we currently removed FLOAT support
    -- it "parses GenRandom for FLOAT32" $ do
    --   let input = "GenRandom DIM(2, 3) FLOAT32"
    --   let result = testParser parseMatrixGenerator input
    --   case result of
    --     Right (MatrixVal matrix) -> do
    --       length matrix `shouldBe` 2
    --       all (\row -> length row == 3) matrix `shouldBe` True
    --       all (\row -> all isValidFloat32 row) matrix `shouldBe` True
    --     _ -> expectationFailure "Expected a MatrixVal with 2x3 FLOAT32 values"

    it "fails on invalid dimensions" $ do
      let input = "GenFromVal 42 DIM()"
      let result = testParser parseMatrixGenerator input
      case result of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected to fail on empty dimensions"

    it "fails on missing dimensions" $ do
      let input = "GenFromVal 42"
      let result = testParser parseMatrixGenerator input
      case result of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected to fail on missing dimensions"

-- Helper functions for testing random value ranges
isValidInt8 :: Value -> Bool
isValidInt8 (IntVal n) = n >= -128 && n <= 127
isValidInt8 _ = False

-- Removed for now as we currently removed FLOAT support
-- isValidFloat32 :: Value -> Bool
-- isValidFloat32 (FloatVal n) = n >= -1.0e30 && n <= 1.0e30
-- isValidFloat32 _ = False