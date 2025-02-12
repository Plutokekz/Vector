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
  = Assign String Expression
  | Call String
  | Read String
  | Write Expression
  | Begin [Statement]
  | If Condition Statement
  | While Condition Statement

data Condition
  = Eq Expression Expression
  | Lt Expression Expression
  | Gt Expression Expression
  | Not Condition

data Expression
  = Var String
  | Num Int
  | Plus Expression Expression
  | Minus Expression Expression
  | Times Expression Expression
  | Divide Expression Expression
  | Neg Expression