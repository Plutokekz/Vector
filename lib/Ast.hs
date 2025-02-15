module Ast where

-- | AST types that match the grammar
data Program = Program String Block deriving (Show)

data Block = Block
  { constDecls :: [(String, Type, Value)],
    varDecls :: [(String, Type)],
    procedures :: [Procedure],
    instruction :: Statement
  }
  deriving (Show)

data Procedure = Procedure String Block deriving (Show)

data Statement
  = Assignment String Expression
  | Call String
  | Read String
  | Write Expression
  | Compound [Statement]
  | If Condition Statement
  | While Condition Statement
  deriving (Show)

data Condition
  = Compare Expression CompOp Expression
  | Not Condition
  deriving (Show)

data CompOp = Eq | Lt | Gt | Lte | Gte | Neq deriving (Show)

data Expression
  = Binary BinOp Expression Expression
  | Unary UnOp Expression
  | Factor Factor
  deriving (Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | MatrixMul -- @ operator
  | ElementMul -- .* operator
  | ElementDiv -- ./ operator
  deriving (Show)

data UnOp = Pos | Neg deriving (Show)

data Factor
  = Var String
  | IntLit Integer
  | FloatLit Double
  | Parens Expression
  | MatrixLit [[Expression]]
  | MatrixIndex String (Expression, Expression) -- Add support for matrix indexing
  deriving (Show)

data Type
  = IntType IntSize
  | FloatType FloatSize
  | MatrixType
      { baseType :: Type,
        dimensions :: (Integer, Integer),
        matrixSpec :: Maybe MatrixSpecifier
      }
  deriving (Show, Eq)

data IntSize = Int8 | Int16 | Int32 | Int64 | Int128 deriving (Show, Eq)

data FloatSize = Float8 | Float16 | Float32 | Float64 | Float128 deriving (Show, Eq)

data MatrixSpecifier
  = Sparse
  | Identity
  | Diagonal
  | UpperTriangular
  | LowerTriangular
  | Orthogonal
  deriving (Show, Eq)

data Value = IntVal Integer | FloatVal Double deriving (Show)