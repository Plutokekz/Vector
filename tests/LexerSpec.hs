module LexerSpec (spec) where

import Lexer
  ( Lexer (runLexer),
    LexerError (..),
    LexerErrorType (..),
    identifier,
    matchInputWithError,
    matchString,
    number,
    operator,
    runLexer,
    whitespace,
  )
import Test.Hspec
import Token (TokenKeyword (..))

spec :: Spec
spec = do
  describe "testmatchInputWithError" testmatchInputWithError
  describe "testmatchString" testmatchString
  describe "testNumber" testNumber
  describe "testIdentifier" testIdentifier
  describe "testOperator" testOperator
  describe "testWhitespace" testWhitespace

testmatchInputWithError :: Spec
testmatchInputWithError = do
  describe "Lexer.matchInputWithError" $ do
    it "returns new lexer which results in EndOfInput" $ do
      runLexer (matchInputWithError Unexpected ('c' ==)) "" 0 `shouldBe` Left [LexerError 0 EndOfInput]
    it "returns new lexer which results in Unexpected" $ do
      runLexer (matchInputWithError Unexpected ('c' ==)) "x" 0 `shouldBe` Left [LexerError 0 (Unexpected 'x')]
    it "returns new lexer which results in Right with parsed char" $ do
      runLexer (matchInputWithError Unexpected ('c' ==)) "c" 0 `shouldBe` Right (1, 'c', "")

testmatchString :: Spec
testmatchString = do
  describe "Lexer.matchString" $ do
    it "returns new lexer which parses the given matchString" $ do
      runLexer (matchString "Hallo") "Hallo Welt" 0 `shouldBe` Right (5, "Hallo", " Welt")
    it "returns new lexer which parses the given matchString resulting in unexpected token" $ do
      runLexer (matchString "Hallo") "Welt Hallo" 0 `shouldBe` Left [LexerError 0 (Expected 'H' 'W')]
    it "returns new lexer which parses the given matchString resulting in EndOfInput" $ do
      runLexer (matchString "Hallo") "" 0 `shouldBe` Left [LexerError 0 EndOfInput]

testNumber :: Spec
testNumber = do
  describe "Lexer.number" $ do
    it "returns integer number token" $ do
      runLexer number "1234567890" 0 `shouldBe` Right (10, INumber 1234567890, "")
    it "returns double number token" $ do
      runLexer number "1.234567890" 0 `shouldBe` Right (11, FNumber 1.234567890, "")
    it "returns double number token" $ do
      runLexer number "0.001234567890" 0 `shouldBe` Right (14, FNumber 1.23456789e-3, "")
    it "returns EndofInput" $ do
      runLexer number "" 0 `shouldBe` Left [LexerError 0 EndOfInput]
    it "returns Unexpected" $ do
      runLexer number "Hallo Welt" 0 `shouldBe` Left [LexerError 0 (ExpectedNumeric 'H')]

testIdentifier :: Spec
testIdentifier = do
  describe "Lexer.identifier" $ do
    it "returns PROGRAM token" $ do
      runLexer identifier "PROGRAM" 0 `shouldBe` Right (7, PROGRAM, "")
    it "returns CONST token" $ do
      runLexer identifier "CONST" 0 `shouldBe` Right (5, CONST, "")
    it "returns VAR token" $ do
      runLexer identifier "VAR" 0 `shouldBe` Right (3, VAR, "")
    it "returns PROCEDURE token" $ do
      runLexer identifier "PROCEDURE" 0 `shouldBe` Right (9, PROCEDURE, "")
    it "returns CALL token" $ do
      runLexer identifier "CALL" 0 `shouldBe` Right (4, CALL, "")
    it "returns READ token" $ do
      runLexer identifier "READ" 0 `shouldBe` Right (4, READ, "")
    it "returns WRITE token" $ do
      runLexer identifier "WRITE" 0 `shouldBe` Right (5, WRITE, "")
    it "returns BEGIN token" $ do
      runLexer identifier "BEGIN" 0 `shouldBe` Right (5, BEGIN, "")
    it "returns IF token" $ do
      runLexer identifier "IF" 0 `shouldBe` Right (2, IF, "")
    it "returns THEN token" $ do
      runLexer identifier "THEN" 0 `shouldBe` Right (4, THEN, "")
    it "returns WHILE token" $ do
      runLexer identifier "WHILE" 0 `shouldBe` Right (5, WHILE, "")
    it "returns DO token" $ do
      runLexer identifier "DO" 0 `shouldBe` Right (2, DO, "")
    it "returns NOT token" $ do
      runLexer identifier "NOT" 0 `shouldBe` Right (3, NOT, "")
    it "returns INT8 token" $ do
      runLexer identifier "INT8" 0 `shouldBe` Right (4, INT8, "")
    it "returns INT16 token" $ do
      runLexer identifier "INT16" 0 `shouldBe` Right (5, INT16, "")
    it "returns INT32 token" $ do
      runLexer identifier "INT32" 0 `shouldBe` Right (5, INT32, "")
    it "returns INT64 token" $ do
      runLexer identifier "INT64" 0 `shouldBe` Right (5, INT64, "")
    it "returns INT128 token" $ do
      runLexer identifier "INT128" 0 `shouldBe` Right (6, INT128, "")
    it "returns FLOAT8 token" $ do
      runLexer identifier "FLOAT8" 0 `shouldBe` Right (6, FLOAT8, "")
    it "returns FLOAT16 token" $ do
      runLexer identifier "FLOAT16" 0 `shouldBe` Right (7, FLOAT16, "")
    it "returns FLOAT32 token" $ do
      runLexer identifier "FLOAT32" 0 `shouldBe` Right (7, FLOAT32, "")
    it "returns FLOAT64 token" $ do
      runLexer identifier "FLOAT64" 0 `shouldBe` Right (7, FLOAT64, "")
    it "returns FLOAT128 token" $ do
      runLexer identifier "FLOAT128" 0 `shouldBe` Right (8, FLOAT128, "")
    it "returns Sparse token" $ do
      runLexer identifier "Sparse" 0 `shouldBe` Right (6, Sparse, "")
    it "returns integer number token" $ do
      runLexer identifier "Identity" 0 `shouldBe` Right (8, Identity, "")
    it "returns Diagonal token" $ do
      runLexer identifier "Diagonal" 0 `shouldBe` Right (8, Diagonal, "")
    it "returns Orthogonal token" $ do
      runLexer identifier "Orthogonal" 0 `shouldBe` Right (10, Orthogonal, "")
    it "returns LowerTriangular token" $ do
      runLexer identifier "LowerTriangular" 0 `shouldBe` Right (15, LowerTriangular, "")
    it "returns END token" $ do
      runLexer identifier "END" 0 `shouldBe` Right (3, END, "")
    it "returns identifier token" $ do
      runLexer identifier "matrixA test, 123" 0 `shouldBe` Right (7, Identifier "matrixA", " test, 123")

testOperator :: Spec
testOperator = do
  describe "Lexer.operator" $ do
    it "returns Dot token" $ do
      runLexer operator "." 0 `shouldBe` Right (1, Dot, "")
    it "returns ElementMult token" $ do
      runLexer operator ".*" 0 `shouldBe` Right (2, ElementMult, "")
    it "returns ElementDiv token" $ do
      runLexer operator "./" 0 `shouldBe` Right (2, ElementDiv, "")
    it "returns LessThen token" $ do
      runLexer operator "<" 0 `shouldBe` Right (1, LessThen, "")
    it "returns LTE token" $ do
      runLexer operator "<=" 0 `shouldBe` Right (2, LTE, "")
    it "returns GreaterThen token" $ do
      runLexer operator ">" 0 `shouldBe` Right (1, GreaterThen, "")
    it "returns GTE token" $ do
      runLexer operator ">=" 0 `shouldBe` Right (2, GTE, "")
    it "returns Equals token" $ do
      runLexer operator "=" 0 `shouldBe` Right (1, Equals, "")
    it "returns LParent token" $ do
      runLexer operator "(" 0 `shouldBe` Right (1, LParent, "")
    it "returns RParent token" $ do
      runLexer operator ")" 0 `shouldBe` Right (1, RParent, "")
    it "returns Comma token" $ do
      runLexer operator "," 0 `shouldBe` Right (1, Comma, "")
    it "returns LBracket token" $ do
      runLexer operator "[" 0 `shouldBe` Right (1, LBracket, "")
    it "returns RBracket token" $ do
      runLexer operator "]" 0 `shouldBe` Right (1, RBracket, "")
    it "returns SemiColon token" $ do
      runLexer operator ";" 0 `shouldBe` Right (1, SemiColon, "")
    it "returns Colon token" $ do
      runLexer operator ":" 0 `shouldBe` Right (1, Colon, "")
    it "returns Transpose token" $ do
      runLexer operator "\'" 0 `shouldBe` Right (1, Transpose, "")

testWhitespace :: Spec
testWhitespace = do
  describe "Lexer.whitespace" $ do
    it "skips whitespace returns empty tuple" $ do
      runLexer whitespace " " 0 `shouldBe` Right (1, (), "")
