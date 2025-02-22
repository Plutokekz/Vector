module ParserSpec (spec) where

import Ast
import Control.Monad.State
import Control.Monad.Except
import Data.Map qualified as Map
import SimpleParser
import Test.Hspec
import Token (Token (..), TokenKeyword (..))
import Lexer (tokenize)

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
      result `shouldBe` Right (Block [] [] [] (Compound [
        Assignment "x" (Factor (IntLit 1)),
        Assignment "y" (Factor (IntLit 2)),
        Assignment "z" (Factor (IntLit 3))
        ]))

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
      result `shouldBe` Right [
        ("x", NumberType (IntType Int32), IntVal 42),
        ("y", NumberType (IntType Int32), IntVal 24)
        ]

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
      result `shouldBe` Right [
        ("x", NumberType (IntType Int32)),
        ("y", NumberType (FloatType Float64))
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
      result `shouldBe` Right (VectorizedType (IntType Int32) [10, 20] Nothing)

    it "parses vector types with specifier" $ do
      let input = "INT32 DIM(10, 10) Identity"
      let result = testParser parseType input
      result `shouldBe` Right (VectorizedType (IntType Int32) [10, 10] (Just Ast.Identity))

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
      result `shouldBe` Right (While (Compare (Factor (Var "x")) Gt (Factor (IntLit 0))) 
        (Compound [Assignment "x" (Binary Sub (Factor (Var "x")) (Factor (IntLit 1)))]))

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
      result `shouldBe` Right (Factor (VectorizedLit [[Factor (IntLit 3)]]))

    it "parses multi-element vector literal" $ do
      let input = "[1, 2, 3]"
      let result = testParser parseExpression input
      result `shouldBe` Right (Factor (VectorizedLit [[Factor (IntLit 1), Factor (IntLit 2), Factor (IntLit 3)]]))

    it "parses matrix literal" $ do
      let input = "[[1, 2], [3, 4]]"
      let result = testParser parseExpression input
      result `shouldBe` Right (Factor (VectorizedLit [[Factor (IntLit 1), Factor (IntLit 2)], [Factor (IntLit 3), Factor (IntLit 4)]]))

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