module Main where

import Lexer (tokenize)

x :: String
x = "PROGRAMM test; VAR x = 5;"

y :: String
y = "PROGRAMM test2; VAR x = 5; VAR A: Sparse[INT32]; VAR B: Sparse[INT32]; \
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
z = "PROGRAMM matrixOps;\
    \VAR\
    \  A: Sparse[INT32];\
    \  B: Identity[FLOAT64];\
    \  C: Diagonal[INT8];\
    \BEGIN\
    \  -- Matrix initialization\
    \  A = [[1, 0, 3],\
    \       [0, 2, 0],\
    \       [4, 0, 5]];\
    \  B = [[1, 0, 0],\
    \       [0, 1, 0],\
    \       [0, 0, 1]];\
    \  \
    \  -- Matrix operations\
    \  D = A @ B;           -- matrix multiplication\
    \  E = A';              -- transpose\
    \  F = A .+ B;          -- element-wise addition\
    \  G = A .* B;          -- element-wise multiplication\
    \  H = A .- B;          -- element-wise subtraction\
    \  I = A ./ B;          -- element-wise division\
    \  \
    \  -- Comparisons\
    \  IF A <= B THEN\
    \    WRITE A;\
    \  IF A >= B THEN\
    \    WRITE B;\
    \  \
    \  -- Matrix types\
    \  J: LowerTriangular[INT32];\
    \  K: UpperTriangular[FLOAT64];\
    \  L: Orthogonal[INT128];\
    \"

main :: IO ()
main = do
  let tokenx = tokenize x
  let tokeny = tokenize y
  let tokenz = tokenize z
  print tokenx
  print tokeny
  print tokenz
