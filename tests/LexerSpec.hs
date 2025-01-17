module LexerSpec (spec) where

import Lexer (LexerError (..), LexerErrorType (..), runLexer, string, token')
import Test.Hspec

spec :: Spec
spec = do
  describe "testToken" testToken'
  describe "testString" testString

testToken' :: Spec
testToken' = do
  describe "Lexer.token'" $ do
    it "returns new lexer which results in EndOfInput" $ do
      runLexer (token' Unexpected ('c' ==)) "" 0 `shouldBe` Left [LexerError 0 EndOfInput]
    it "returns new lexer which results in Unexpected" $ do
      runLexer (token' Unexpected ('c' ==)) "x" 0 `shouldBe` Left [LexerError 0 (Unexpected 'x')]
    it "returns new lexer which results in Right with parsed char" $ do
      runLexer (token' Unexpected ('c' ==)) "c" 0 `shouldBe` Right (1, 'c', "")

testString :: Spec
testString = do
  describe "Lexer.string" $ do
    it "returns new lexer which parses the given string" $ do
      runLexer (string "Hallo") "Hallo Welt" 0 `shouldBe` Right (5, "Hallo", " Welt")
    it "returns new lexer which parses the given string resulting in unexpected token" $ do
      runLexer (string "Hallo") "Welt Hallo" 0 `shouldBe` Left [LexerError 0 (Expected 'H' 'W')]
    it "returns new lexer which parses the given string resulting in EndOfInput" $ do
      runLexer (string "Hallo") "" 0 `shouldBe` Left [LexerError 0 EndOfInput]