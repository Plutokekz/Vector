import Data.Char (isDigit)

data Token
  = PROGRAMM
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
  | Equals
  | LessThen
  | GreaterThen
  | LParent
  | RParrent
  | Dot
  | SemiColon
  | RSquareParrent
  | LSquareParrent
  | INT8 Integer
  | INT16 Integer
  | INT32 Integer
  | INT64 Integer
  | INT128 Integer
  | FLOAT8 Double
  | FLOAT16 Double
  | FLOAT32 Double
  | FLOAT64 Double
  | FLOAT128 Double
  | FNumber Double
  | INumber Integer
  | Sparse
  | Identity
  | Diagonal
  | Orthogonal
  | LowerTriangular
  deriving (Show, Eq)

type Lexer = String -> [Maybe Token] -> ([Maybe Token], String)

parseParrent :: Lexer
parseParrent ('(' : xs) tokens = (tokens ++ [Just LParent], xs)
parseParrent (')' : xs) tokens = (tokens ++ [Just RParrent], xs)
parseParrent ('[' : xs) tokens = (tokens ++ [Just LSquareParrent], xs)
parseParrent (']' : xs) tokens = (tokens ++ [Just RSquareParrent], xs)

parseNumber :: String -> Lexer
parseNumber acc "" tokens = (tokens ++ [Nothing], "")
parseNumber acc (x : xs) tokens
  | isDigit x = parseNumber (acc ++ [x]) xs tokens
  | x == '.' = parseDuble (acc ++ [x]) xs tokens
  | otherwise = (tokens ++ [Just (INumber (read acc :: Integer))], xs)

parseDuble :: String -> Lexer
parseDuble acc "" tokens = (tokens ++ [Nothing], "")
parseDuble acc (x : xs) tokens
  | isDigit x = parseDuble (acc ++ [x]) xs tokens
  | otherwise = (tokens ++ [Just (FNumber (read acc :: Double))], xs)