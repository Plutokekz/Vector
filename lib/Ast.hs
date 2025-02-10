module Ast where

data Program = Program String Block

data Block = Block
  { constants :: [(String, Int)],
    variables :: [String],
    procedures :: [Procedure],
    body :: Statement
  }

data Procedure = Procedure
  { procedureName :: String,
    procedureBlock :: Block
  }

data Statement
  = Assign String Expr
  | Call String
  | Read String
  | Write Expr
  | Begin [Statement]
  | If Condition Statement
  | While Condition Statement

data Condition
  = Eq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Not Condition

data Expr
  = Var String
  | Num Int
  | Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Divide Expr Expr
  | Neg Expr