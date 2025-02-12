module SimpleParser
  ( parse,
    ParseError (..),
    Program (..), -- Export our AST types
    Block (..),
    Statement (..),
    Expression (..),
    Type (..),
  )
where

-- Import Lexer qualified
-- Still import TokenPos directly as it's not ambiguous

import Control.Applicative (Alternative (..), many, optional)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Lexer (TokenPos (..))
import qualified Lexer as L

-- | Represents a parsing error with position information
data ParseError = ParseError
  { errorPos :: Int,
    expected :: [L.Token],
    got :: L.Token
  }
  deriving (Show, Eq)

-- | Semigroup instance for ParseError - we keep the first error
instance Semigroup ParseError where
  e1 <> _ = e1 -- Keep the first error

-- | Monoid instance for ParseError - needed for many and some
instance Monoid ParseError where
  mempty = ParseError 0 [] (L.Identifier "") -- Should never be used
  mappend = (<>)

-- | Parser state containing current token position and remaining tokens
data ParserState = ParserState
  { currentToken :: TokenPos,
    remainingTokens :: [TokenPos]
  }
  deriving (Show)

-- | The Parser monad - combines State for token management and Either for error handling
type Parser a = ExceptT ParseError (State ParserState) a

-- | Initialize parser state with a list of tokens
initParserState :: [TokenPos] -> ParserState
initParserState [] = error "Cannot parse empty token stream"
initParserState (t : ts) = ParserState t ts

-- | Main parsing function that takes a list of tokens and returns either an error or AST
parse :: [TokenPos] -> Either ParseError Program
parse tokens = evalState (runExceptT parseProgram) (initParserState tokens)

-- | Get the current token without consuming it
peek :: Parser L.Token
peek = gets (L.token . currentToken)

-- | Consume and return the current token, advancing to the next one
advance :: Parser L.Token
advance = do
  current <- gets currentToken
  remaining <- gets remainingTokens
  case remaining of
    [] -> return $ L.token current -- At end of input
    (next : rest) -> do
      put $ ParserState next rest
      return $ L.token current

-- | Match a specific token, consuming it if it matches
match :: L.Token -> Parser L.Token
match expected = do
  actual <- peek
  current <- gets currentToken
  if actual == expected
    then advance
    else
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [expected],
            got = actual
          }

-- | Parse a program according to the grammar
parseProgram :: Parser Program
parseProgram = do
  match L.PROGRAMM
  name <- parseIdentifier
  match L.Colon
  block <- parseBlock
  match L.Dot
  return $ Program name block

-- | Parse an identifier
parseIdentifier :: Parser String
parseIdentifier = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.Identifier name -> do
      advance
      return name
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.Identifier ""],
            got = tok
          }

-- | Parse a block
parseBlock :: Parser Block
parseBlock = do
  consts <- parseConstDecls
  vars <- parseVarDecls
  procs <- parseProcDecls
  instr <- parseInstruction
  return $ Block consts vars procs instr

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
  deriving (Show)

data IntSize = Int8 | Int16 | Int32 | Int64 | Int128 deriving (Show)

data FloatSize = Float8 | Float16 | Float32 | Float64 | Float128 deriving (Show)

data MatrixSpecifier
  = Sparse
  | Identity
  | Diagonal
  | UpperTriangular
  | LowerTriangular
  | Orthogonal
  deriving (Show)

data Value = IntVal Integer | FloatVal Double deriving (Show)

-- | Parse constant declarations
parseConstDecls :: Parser [(String, Type, Value)]
parseConstDecls = do
  tok <- peek
  case tok of
    L.CONST -> do
      advance
      typ <- parseType
      name <- parseIdentifier
      match L.Equals
      val <- parseValue
      rest <-
        many
          ( do
              match L.Comma
              name' <- parseIdentifier
              match L.Equals
              val' <- parseValue
              return (name', typ, val')
          )
      match L.SemiColon
      return $ (name, typ, val) : rest
    _ -> return []

-- | Parse variable declarations
parseVarDecls :: Parser [(String, Type)]
parseVarDecls = do
  tok <- peek
  case tok of
    L.VAR -> do
      advance
      let parseOneDecl = do
            baseType <- parseType
            name <- parseIdentifier
            -- If it's a matrix type, parse the optional specifier here
            case baseType of
              MatrixType {} -> do
                mspec <- optional parseMatrixType
                match L.SemiColon
                return (name, baseType {matrixSpec = mspec})
              _ -> do
                match L.SemiColon
                return (name, baseType)
      -- Parse one or more declarations
      decls <- parseMany parseOneDecl
      -- Look ahead to see if there's another VAR declaration
      nextDecls <- parseVarDecls
      return $ decls ++ nextDecls
    _ -> return []

-- | Parse procedure declarations
parseProcDecls :: Parser [Procedure]
parseProcDecls = many $ do
  tok <- peek
  case tok of
    L.PROCEDURE -> do
      advance
      name <- parseIdentifier
      match L.SemiColon
      block <- parseBlock
      match L.SemiColon
      return $ Procedure name block
    _ -> empty -- Use empty from Alternative instead of return []

-- | Parse a statement
parseInstruction :: Parser Statement
parseInstruction = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.BEGIN -> parseCompound
    L.CALL -> parseCall
    L.READ -> parseRead
    L.WRITE -> parseWrite
    L.IF -> parseIf
    L.WHILE -> parseWhile
    L.Identifier _ -> parseAssignment
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.BEGIN, L.CALL, L.READ, L.WRITE, L.IF, L.WHILE, L.Identifier ""],
            got = tok
          }

-- | Parse a compound statement
parseCompound :: Parser Statement
parseCompound = do
  match L.BEGIN
  stmts <- parseInstruction `sepBy1` (match L.SemiColon)
  match L.END
  return $ Compound stmts

-- Helper function for parsing separated by
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  return (x : xs)

-- | Parse a type
parseType :: Parser Type
parseType = do
  tok <- peek
  current <- gets currentToken
  baseType <- case tok of
    L.INT8 -> advance >> return (IntType Int8)
    L.INT16 -> advance >> return (IntType Int16)
    L.INT32 -> advance >> return (IntType Int32)
    L.INT64 -> advance >> return (IntType Int64)
    L.INT128 -> advance >> return (IntType Int128)
    L.FLOAT8 -> advance >> return (FloatType Float8)
    L.FLOAT16 -> advance >> return (FloatType Float16)
    L.FLOAT32 -> advance >> return (FloatType Float32)
    L.FLOAT64 -> advance >> return (FloatType Float64)
    L.FLOAT128 -> advance >> return (FloatType Float128)
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.INT8, L.INT16, L.INT32, L.INT64, L.INT128, L.FLOAT8, L.FLOAT16, L.FLOAT32, L.FLOAT64, L.FLOAT128],
            got = tok
          }

  -- Check for matrix type
  matrixType <- optional $ do
    match L.LBracket
    -- Parse dimensions (e.g., [3,3])
    size1 <- parseNumber
    match L.Comma
    size2 <- parseNumber
    match L.RBracket
    return $
      MatrixType
        { baseType = baseType,
          dimensions = (size1, size2),
          matrixSpec = Nothing -- Matrix specifier will be parsed later
        }

  return $ case matrixType of
    Just mt -> mt
    Nothing -> baseType

-- | Parse a matrix type specifier
parseMatrixType :: Parser MatrixSpecifier
parseMatrixType = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.Sparse -> advance >> return Sparse
    L.Identity -> advance >> return Identity
    L.Diagonal -> advance >> return Diagonal
    L.UpperTriangular -> advance >> return UpperTriangular
    L.LowerTriangular -> advance >> return LowerTriangular
    L.Orthogonal -> advance >> return Orthogonal
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.Sparse, L.Identity, L.Diagonal, L.UpperTriangular, L.LowerTriangular, L.Orthogonal],
            got = tok
          }

-- | Parse a value (for constants)
parseValue :: Parser Value
parseValue = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.INumber n -> advance >> return (IntVal n)
    L.FNumber n -> advance >> return (FloatVal n)
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.INumber 0, L.FNumber 0.0],
            got = tok
          }

-- | Parse a number token
parseNumber :: Parser Integer
parseNumber = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.INumber n -> advance >> return n
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.INumber 0],
            got = tok
          }

-- | Parse an assignment statement
parseAssignment :: Parser Statement
parseAssignment = do
  name <- parseIdentifier
  match L.Equals
  expr <- parseExpression
  return $ Assignment name expr

-- | Parse a call statement
parseCall :: Parser Statement
parseCall = do
  match L.CALL
  name <- parseIdentifier
  return $ Call name

-- | Parse a read statement
parseRead :: Parser Statement
parseRead = do
  match L.READ
  name <- parseIdentifier
  return $ Read name

-- | Parse a write statement
parseWrite :: Parser Statement
parseWrite = do
  match L.WRITE
  expr <- parseExpression
  return $ Write expr

-- | Parse an if statement
parseIf :: Parser Statement
parseIf = do
  match L.IF
  cond <- parseCondition
  match L.THEN
  stmt <- parseInstruction
  return $ If cond stmt

-- | Parse a while statement
parseWhile :: Parser Statement
parseWhile = do
  match L.WHILE
  cond <- parseCondition
  match L.DO
  stmt <- parseInstruction
  return $ While cond stmt

-- | Parse a condition
parseCondition :: Parser Condition
parseCondition = do
  tok <- peek
  case tok of
    L.NOT -> do
      advance
      cond <- parseCondition
      return $ Not cond
    _ -> do
      expr1 <- parseExpression
      op <- parseCompOp
      expr2 <- parseExpression
      return $ Compare expr1 op expr2

-- | Parse a comparison operator
parseCompOp :: Parser CompOp
parseCompOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.Equals -> advance >> return Eq
    L.LessThen -> do
      advance
      hasEq <- optional $ match L.Equals
      return $ case hasEq of
        Just _ -> Lte
        Nothing -> Lt
    L.GreaterThen -> do
      advance
      hasEq <- optional $ match L.Equals
      return $ case hasEq of
        Just _ -> Gte
        Nothing -> Gt
    L.NotEqual -> advance >> return Neq
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.Equals, L.LessThen, L.GreaterThen, L.NotEqual],
            got = tok
          }

-- | Parse an expression
parseExpression :: Parser Expression
parseExpression = do
  sign <- optional (char '+' <|> char '-')
  term <- parseTerm
  let initial = case sign of
        Just '-' -> Unary Neg term
        _ -> term
  rest <- many $ do
    op <- parseAddOp
    term' <- parseTerm
    return (op, term')
  return $ foldl (\acc (op, t) -> Binary op acc t) initial rest

-- | Parse a term
parseTerm :: Parser Expression
parseTerm = do
  factor <- parseFactor
  rest <- many $ do
    op <- parseMulOp <|> parseMatrixOp
    factor' <- parseFactor
    return (op, factor')
  return $ foldl (\acc (op, f) -> Binary op acc f) factor rest

-- | Parse a factor
parseFactor :: Parser Expression
parseFactor = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.LParent -> do
      advance
      expr <- parseExpression
      match L.RParent
      return $ Factor $ Parens expr
    L.LBracket -> do
      -- Handle matrix literals
      advance
      match L.LBracket -- Expect second opening bracket
      -- Parse first row
      row1 <- parseMatrixRow
      match L.RBracket -- Close first row
      rows <- many $ do
        match L.Comma
        match L.LBracket
        row <- parseMatrixRow
        match L.RBracket
        return row
      match L.RBracket -- Close outer bracket
      return $ Factor $ MatrixLit (row1 : rows)
    L.INumber n -> advance >> return (Factor $ IntLit n)
    L.FNumber n -> advance >> return (Factor $ FloatLit n)
    L.Identifier name -> do
      advance
      -- Look ahead for possible matrix indexing
      tok' <- peek
      case tok' of
        L.LBracket -> do
          advance
          idx1 <- parseExpression
          match L.Comma
          idx2 <- parseExpression
          match L.RBracket
          return $ Factor $ MatrixIndex name (idx1, idx2)
        _ -> return $ Factor $ Var name
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.LParent, L.LBracket, L.INumber 0, L.FNumber 0.0, L.Identifier ""],
            got = tok
          }

-- | Parse a matrix row
parseMatrixRow :: Parser [Expression]
parseMatrixRow = do
  expr <- parseExpression
  rest <- many $ do
    match L.Comma
    parseExpression
  return (expr : rest)

-- | Parse an additive operator
parseAddOp :: Parser BinOp
parseAddOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.Plus -> advance >> return Add
    L.Minus -> advance >> return Sub
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.Plus, L.Minus],
            got = tok
          }

-- | Parse a multiplicative operator
parseMulOp :: Parser BinOp
parseMulOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.Times -> advance >> return Mul
    L.Divide -> advance >> return Div
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.Times, L.Divide],
            got = tok
          }

-- | Parse a matrix operator
parseMatrixOp :: Parser BinOp
parseMatrixOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.MatrixMult -> advance >> return MatrixMul
    L.ElementMult -> advance >> return ElementMul
    L.ElementDiv -> advance >> return ElementDiv
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.MatrixMult, L.ElementMult, L.ElementDiv],
            got = tok
          }

-- Helper for parseMany
parseMany :: Parser a -> Parser [a]
parseMany p = do
  x <- p
  xs <- many p
  return (x : xs)

-- Helper for char
char :: Char -> Parser Char
char c = do
  tok <- peek
  current <- gets currentToken
  case tok of
    L.Plus | c == '+' -> advance >> return c
    L.Minus | c == '-' -> advance >> return c
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [L.Plus | c == '+'] ++ [L.Minus | c == '-'],
            got = tok
          }
