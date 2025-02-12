module Token where

type Offset = Int

data LexerError i = LexerError {erOffset :: Offset, erError :: LexerErrorType i} deriving (Eq, Show)

data LexerErrorType i
  = EndOfInput
  | Unexpected i
  | Empty
  | ExpectedEndOfFile i
  | Expected i i
  deriving (Eq, Show)

data TokenPos = TokenPos {tokenOffset :: Offset, token :: Token} deriving (Show, Eq)

data Token
  = PROGRAMM
  -- programm keywords
  | CONST
  | VAR
  | PROCEDURE
  | CALL
  | READ
  | WRITE
  | BEGIN
  | IF
  | THEN
  | WHILE
  | DO
  | NOT
  | Identifier String
  | END
  -- simple comparison operators
  | Equals
  | LessThen
  | GreaterThen
  -- syntactical elements
  | LParent
  | RParent
  | Dot
  | SemiColon
  | Colon
  | Comma
  | LBracket
  | RBracket
  -- number types
  | INT8
  | INT16
  | INT32
  | INT64
  | INT128
  | FLOAT8
  | FLOAT16
  | FLOAT32
  | FLOAT64
  | FLOAT128
  | FNumber Double
  | INumber Integer
  | Minus
  | Plus
  | Times
  | Divide
  -- matrix types
  | Sparse
  | Identity
  | Diagonal
  | Orthogonal
  | LowerTriangular
  | UpperTriangular
  -- simple matrix operators
  | MatrixMult
  | Transpose
  -- compound operators
  -- element wise operators
  | ElementMult
  | ElementDiv
  -- compound comparison operators (less than or equal to...)
  | LTE
  | GTE
  | NotEqual
  -- logical operators
  | True
  | False
  | And
  | Or
  deriving (Show, Eq)
