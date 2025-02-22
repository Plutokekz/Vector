module Token where

type Offset = Int

data Token = Token
  { tokenOffset :: Offset,
    tokenKeyword :: TokenKeyword
  }
  deriving (Show, Eq)

data TokenKeyword
  = PROGRAM
  | -- programm keywords
    CONST
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
  | -- Assignment operator
    Equals
  | -- simple comparison operators
    LessThen
  | GreaterThen
  | -- compound comparison operators (less than or equal to...)
    IsEqual
  | LTE
  | GTE
  | NotEqual
  | -- syntactical elements
    LParent
  | RParent
  | Dot
  | SemiColon
  | Colon
  | Comma
  | LBracket
  | RBracket
  | -- required to specify matrix dimensions
    DIM
  | -- number types
    INT8
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
  | -- matrix types
    Sparse
  | Identity
  | Diagonal
  | Orthogonal
  | LowerTriangular
  | UpperTriangular
  | -- simple matrix operators
    MatrixMult
  | Transpose
  | -- compound operators
    -- element wise operators
    ElementMult
  | ElementDiv
  | -- logical operators
    T
  | F
  | And
  | Or
  | -- arithmetic operators
    Plus
  | Minus
  | Times
  | Divide
  deriving (Show, Eq, Ord)
