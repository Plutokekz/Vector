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
import qualified Token as T
import Ast

-- | Represents a parsing error with position information
data ParseError = ParseError
  { errorPos :: Int,
    expected :: [T.Token],
    got :: T.Token
  }
  deriving (Show, Eq)

-- | Semigroup instance for ParseError - we keep the first error
instance Semigroup ParseError where
  e1 <> _ = e1 -- Keep the first error

-- | Monoid instance for ParseError - needed for many and some
instance Monoid ParseError where
  mempty = ParseError 0 [] (T.Identifier "") -- Should never be used
  mappend = (<>)

-- | Parser state containing current token position and remaining tokens
data ParserState = ParserState
  { currentToken :: T.TokenPos,
    remainingTokens :: [T.TokenPos]
  }
  deriving (Show)

-- | The Parser monad - combines State for token management and Either for error handling
type Parser a = ExceptT ParseError (State ParserState) a

-- | Initialize parser state with a list of tokens
initParserState :: [T.TokenPos] -> ParserState
initParserState [] = error "Cannot parse empty token stream"
initParserState (t : ts) = ParserState t ts

-- | Main parsing function that takes a list of tokens and returns either an error or AST
parse :: [T.TokenPos] -> Either ParseError Program
parse tokens = evalState (runExceptT parseProgram) (initParserState tokens)

-- | Get the current token without consuming it
peek :: Parser T.Token
peek = gets (T.token . currentToken)

-- | Consume and return the current token, advancing to the next one
advance :: Parser T.Token
advance = do
  current <- gets currentToken
  remaining <- gets remainingTokens
  case remaining of
    [] -> return $ T.token current -- At end of input
    (next : rest) -> do
      put $ ParserState next rest
      return $ T.token current

-- | Match a specific token, consuming it if it matches
match :: T.Token -> Parser T.Token
match expected = do
  actual <- peek
  current <- gets currentToken
  if actual == expected
    then advance
    else
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [expected],
            got = actual
          }

-- | Parse a program according to the grammar
parseProgram :: Parser Program
parseProgram = do
  match T.PROGRAMM
  name <- parseIdentifier
  match T.Colon
  block <- parseBlock
  match T.Dot
  return $ Program name block

-- | Parse an identifier
parseIdentifier :: Parser String
parseIdentifier = do
  tok <- peek
  current <- gets currentToken
  case tok of
    T.Identifier name -> do
      advance
      return name
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.Identifier ""],
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

-- | Parse constant declarations
parseConstDecls :: Parser [(String, Type, Value)]
parseConstDecls = do
  tok <- peek
  case tok of
    T.CONST -> do
      advance
      typ <- parseType
      name <- parseIdentifier
      match T.Equals
      val <- parseValue
      rest <-
        many
          ( do
              match T.Comma
              name' <- parseIdentifier
              match T.Equals
              val' <- parseValue
              return (name', typ, val')
          )
      match T.SemiColon
      return $ (name, typ, val) : rest
    _ -> return []

-- | Parse variable declarations
parseVarDecls :: Parser [(String, Type)]
parseVarDecls = do
  tok <- peek
  case tok of
    T.VAR -> do
      advance
      let parseOneDecl = do
            baseType <- parseType
            name <- parseIdentifier
            -- If it's a matrix type, parse the optional specifier here
            case baseType of
              MatrixType {} -> do
                mspec <- optional parseMatrixType
                match T.SemiColon
                return (name, baseType {matrixSpec = mspec})
              _ -> do
                match T.SemiColon
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
    T.PROCEDURE -> do
      advance
      name <- parseIdentifier
      match T.SemiColon
      block <- parseBlock
      match T.SemiColon
      return $ Procedure name block
    _ -> empty -- Use empty from Alternative instead of return []

-- | Parse a statement
parseInstruction :: Parser Statement
parseInstruction = do
  tok <- peek
  current <- gets currentToken
  case tok of
    T.BEGIN -> parseCompound
    T.CALL -> parseCall
    T.READ -> parseRead
    T.WRITE -> parseWrite
    T.IF -> parseIf
    T.WHILE -> parseWhile
    T.Identifier _ -> parseAssignment
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.BEGIN, T.CALL, T.READ, T.WRITE, T.IF, T.WHILE, T.Identifier ""],
            got = tok
          }

-- | Parse a compound statement
parseCompound :: Parser Statement
parseCompound = do
  match T.BEGIN
  stmts <- parseInstruction `sepBy1` (match T.SemiColon)
  match T.END
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
    T.INT8 -> advance >> return (IntType Int8)
    T.INT16 -> advance >> return (IntType Int16)
    T.INT32 -> advance >> return (IntType Int32)
    T.INT64 -> advance >> return (IntType Int64)
    T.INT128 -> advance >> return (IntType Int128)
    T.FLOAT8 -> advance >> return (FloatType Float8)
    T.FLOAT16 -> advance >> return (FloatType Float16)
    T.FLOAT32 -> advance >> return (FloatType Float32)
    T.FLOAT64 -> advance >> return (FloatType Float64)
    T.FLOAT128 -> advance >> return (FloatType Float128)
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.INT8, T.INT16, T.INT32, T.INT64, T.INT128, T.FLOAT8, T.FLOAT16, T.FLOAT32, T.FLOAT64, T.FLOAT128],
            got = tok
          }

  -- Check for matrix type
  matrixType <- optional $ do
    match T.LBracket
    -- Parse dimensions (e.g., [3,3])
    size1 <- parseNumber
    match T.Comma
    size2 <- parseNumber
    match T.RBracket
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
    T.Sparse -> advance >> return Sparse
    T.Identity -> advance >> return Identity
    T.Diagonal -> advance >> return Diagonal
    T.UpperTriangular -> advance >> return UpperTriangular
    T.LowerTriangular -> advance >> return LowerTriangular
    T.Orthogonal -> advance >> return Orthogonal
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.Sparse, T.Identity, T.Diagonal, T.UpperTriangular, T.LowerTriangular, T.Orthogonal],
            got = tok
          }

-- | Parse a value (for constants)
parseValue :: Parser Value
parseValue = do
  tok <- peek
  current <- gets currentToken
  case tok of
    T.INumber n -> advance >> return (IntVal n)
    T.FNumber n -> advance >> return (FloatVal n)
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.INumber 0, T.FNumber 0.0],
            got = tok
          }

-- | Parse a number token
parseNumber :: Parser Integer
parseNumber = do
  tok <- peek
  current <- gets currentToken
  case tok of
    T.INumber n -> advance >> return n
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.INumber 0],
            got = tok
          }

-- | Parse an assignment statement
parseAssignment :: Parser Statement
parseAssignment = do
  name <- parseIdentifier
  match T.Equals
  expr <- parseExpression
  return $ Assignment name expr

-- | Parse a call statement
parseCall :: Parser Statement
parseCall = do
  match T.CALL
  name <- parseIdentifier
  return $ Call name

-- | Parse a read statement
parseRead :: Parser Statement
parseRead = do
  match T.READ
  name <- parseIdentifier
  return $ Read name

-- | Parse a write statement
parseWrite :: Parser Statement
parseWrite = do
  match T.WRITE
  expr <- parseExpression
  return $ Write expr

-- | Parse an if statement
parseIf :: Parser Statement
parseIf = do
  match T.IF
  cond <- parseCondition
  match T.THEN
  stmt <- parseInstruction
  return $ If cond stmt

-- | Parse a while statement
parseWhile :: Parser Statement
parseWhile = do
  match T.WHILE
  cond <- parseCondition
  match T.DO
  stmt <- parseInstruction
  return $ While cond stmt

-- | Parse a condition
parseCondition :: Parser Condition
parseCondition = do
  tok <- peek
  case tok of
    T.NOT -> do
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
    T.Equals -> advance >> return Eq
    T.LessThen -> do
      advance
      hasEq <- optional $ match T.Equals
      return $ case hasEq of
        Just _ -> Lte
        Nothing -> Lt
    T.GreaterThen -> do
      advance
      hasEq <- optional $ match T.Equals
      return $ case hasEq of
        Just _ -> Gte
        Nothing -> Gt
    T.NotEqual -> advance >> return Neq
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.Equals, T.LessThen, T.GreaterThen, T.NotEqual],
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
    T.LParent -> do
      advance
      expr <- parseExpression
      match T.RParent
      return $ Factor $ Parens expr
    T.LBracket -> do
      -- Handle matrix literals
      advance
      match T.LBracket -- Expect second opening bracket
      -- Parse first row
      row1 <- parseMatrixRow
      match T.RBracket -- Close first row
      rows <- many $ do
        match T.Comma
        match T.LBracket
        row <- parseMatrixRow
        match T.RBracket
        return row
      match T.RBracket -- Close outer bracket
      return $ Factor $ MatrixLit (row1 : rows)
    T.INumber n -> advance >> return (Factor $ IntLit n)
    T.FNumber n -> advance >> return (Factor $ FloatLit n)
    T.Identifier name -> do
      advance
      -- Look ahead for possible matrix indexing
      tok' <- peek
      case tok' of
        T.LBracket -> do
          advance
          idx1 <- parseExpression
          match T.Comma
          idx2 <- parseExpression
          match T.RBracket
          return $ Factor $ MatrixIndex name (idx1, idx2)
        _ -> return $ Factor $ Var name
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.LParent, T.LBracket, T.INumber 0, T.FNumber 0.0, T.Identifier ""],
            got = tok
          }

-- | Parse a matrix row
parseMatrixRow :: Parser [Expression]
parseMatrixRow = do
  expr <- parseExpression
  rest <- many $ do
    match T.Comma
    parseExpression
  return (expr : rest)

-- | Parse an additive operator
parseAddOp :: Parser BinOp
parseAddOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    T.Plus -> advance >> return Add
    T.Minus -> advance >> return Sub
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.Plus, T.Minus],
            got = tok
          }

-- | Parse a multiplicative operator
parseMulOp :: Parser BinOp
parseMulOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    T.Times -> advance >> return Mul
    T.Divide -> advance >> return Div
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.Times, T.Divide],
            got = tok
          }

-- | Parse a matrix operator
parseMatrixOp :: Parser BinOp
parseMatrixOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    T.MatrixMult -> advance >> return MatrixMul
    T.ElementMult -> advance >> return ElementMul
    T.ElementDiv -> advance >> return ElementDiv
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.MatrixMult, T.ElementMult, T.ElementDiv],
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
    T.Plus | c == '+' -> advance >> return c
    T.Minus | c == '-' -> advance >> return c
    _ ->
      throwError $
        ParseError
          { errorPos = T.tokenOffset current,
            expected = [T.Plus | c == '+'] ++ [T.Minus | c == '-'],
            got = tok
          }
