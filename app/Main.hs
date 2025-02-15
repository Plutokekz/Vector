-- module Main where
-- import Lexer (tokenize)
-- import qualified SimpleParser as SP  -- New parser

module Main where

import Lexer (tokenize)
import SimpleParser qualified as SP -- New parser
import Token (Token (..), TokenPos (..))

x :: String
x = "PROGRAMM test; VAR x = 5;"

y :: String
y =
  "PROGRAMM test2; VAR x = 5; VAR A: Sparse[INT32]; VAR B: Sparse[INT32]; \
  \BEGIN\
  \  A = [[1, 0, 3],\
  \       [0, 2, 0],\
  \       [4, 0, 5]];\
  \  B = [[1, 0, 0],\
  \       [0, 1, 0],\
  \       [0, 0, 1]];\
  \C = A .* B\
  \END"

z :: String
z =
  "PROGRAMM matrixOps;\
  \VAR\
  \  A: Sparse[INT32];\
  \  B: Identity[FLOAT64];\
  \  C: Diagonal[INT8];\
  \BEGIN\
  \  A = [[1, 0, 3],\
  \       [0, 2, 0],\
  \       [4, 0, 5]];\
  \  B = [[1, 0, 0],\
  \       [0, 1, 0],\
  \       [0, 0, 1]];\
  \  \
  \  D = A @ B;           \
  \  E = A';              \
  \  G = A .* B;          \
  \  I = A ./ B;          \
  \  \
  \  IF A <= B THEN\
  \    WRITE A;\
  \  IF A >= B THEN\
  \    WRITE B;\
  \  \
  \  J: LowerTriangular[INT32];\
  \  K: UpperTriangular[FLOAT64];\
  \  L: Orthogonal[INT128];"

-- main :: IO ()
-- main = do
--   let tokenx = tokenize x
--   let tokeny = tokenize y
--   let tokenz = tokenize z
--   --print tokenx
--   --print tokeny
--   print tokenz

simpleProgram :: String
simpleProgram =
  unlines
    [ "PROGRAMM test:",
      "  CONST INT32 n = 5;",
      "  VAR INT32 x;",
      "  BEGIN",
      "    x = n",
      "  END."
    ]

matrixProgram :: String
matrixProgram =
  unlines
    [ "PROGRAMM matrix:",
      "  VAR INT32[3,3] A Sparse;",
      "  VAR INT32[3,3] B Identity;",
      "  BEGIN",
      "    A = [[1, 0, 3],",
      "         [0, 2, 0],",
      "         [4, 0, 5]];",
      "    B = [[1, 0, 0],",
      "         [0, 1, 0],",
      "         [0, 0, 1]];",
      "    WRITE A @ B",
      "  END."
    ]

complexProgram :: String
complexProgram =
  unlines
    [ "PROGRAMM Example1:",
      "VAR INT32 a;",
      "VAR INT32 b;",
      "VAR INT32 pot;",
      "VAR INT32 n;",
      "VAR INT32 fak;",
      "PROCEDURE potenz;",
      "VAR INT32 y;",
      "BEGIN",
      "  pot = 1;",
      "  y = b;",
      "  WHILE NOT y < 1 DO BEGIN",
      "    pot = pot * a;",
      "    y = y - 1",
      "  END",
      "END;",
      "PROCEDURE fakultaet;",
      "IF n > 1 THEN BEGIN",
      "  fak = fak * n;",
      "  n = n - 1;",
      "  CALL fakultaet",
      "END;",
      "BEGIN",
      "  READ a;",
      "  READ b;",
      "  CALL potenz;",
      "  WRITE pot;",
      "  READ n;",
      "  fak = 1;",
      "  CALL fakultaet;",
      "  WRITE fak",
      "END."
    ]

-- Extract tokens from TokenPos
getTokens :: [TokenPos] -> [Token]
getTokens = map token

main :: IO ()
main = do
  putStrLn "Testing simple program with SimpleParser:"
  case tokenize simpleProgram of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right (_, tokenPos, _) -> case SP.parse tokenPos of
      Left err -> putStrLn $ "Parser error: " ++ show err
      Right ast -> print ast

  putStrLn "\nTesting matrix program with SimpleParser:"
  case tokenize matrixProgram of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right (_, tokenPos, _) -> case SP.parse tokenPos of
      Left err -> putStrLn $ "Parser error: " ++ show err
      Right ast -> print ast

  putStrLn "\nTesting complex program with SimpleParser:"
  case tokenize complexProgram of
    Left err -> putStrLn $ "Lexer error: " ++ show err
    Right (_, tokenPos, _) -> case SP.parse tokenPos of
      Left err -> putStrLn $ "Parser error: " ++ show err
      Right ast -> print ast
