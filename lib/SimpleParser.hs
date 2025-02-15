module SimpleParser where

import Control.Applicative (Alternative (..), many, optional)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Token (Token(..), TokenPos(..), tokenOffset, token)
import Ast

-- | Represents a parsing error with position information
data ParseError = ParseError
  { errorPos :: Int,
    expected :: [Token],
    got :: Token
  }
  deriving (Show, Eq)

-- | Semigroup instance for ParseError - we keep the first error
instance Semigroup ParseError where
  e1 <> _ = e1 -- Keep the first error

-- | Monoid instance for ParseError - needed for many and some
instance Monoid ParseError where
  mempty = ParseError 0 [] (Identifier "")
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
peek :: Parser Token
peek = gets (token . currentToken)

-- | Consume and return the current token, advancing to the next one
advance :: Parser Token
advance = do
  current <- gets currentToken
  remaining <- gets remainingTokens
  case remaining of
    [] -> return $ token current -- At end of input
    (next : rest) -> do
      put $ ParserState next rest
      return $ token current

-- | Match a specific token, consuming it if it matches
match :: Token -> Parser Token
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
  match PROGRAMM
  name <- parseIdentifier
  match Colon
  block <- parseBlock
  match Dot
  return $ Program name block

-- | Parse an identifier
parseIdentifier :: Parser String
parseIdentifier = do
  tok <- peek
  current <- gets currentToken
  case tok of
    Identifier name -> do
      advance
      return name
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [Identifier ""],
            got = tok
          }

-- | Parse a block
parseBlock :: Parser Block
parseBlock = do
  consts <- parseConstDecls
  vars <- parseVarDecls
  procs <- parseProcDecls
  Block consts vars procs <$> parseInstruction

-- | Parse constant declarations
parseConstDecls :: Parser [(String, Type, Value)]
parseConstDecls = do
  tok <- peek
  case tok of
    CONST -> do
      advance
      typ <- parseType
      name <- parseIdentifier
      match Equals
      val <- parseValue
      rest <-
        many
          ( do
              match Comma
              name' <- parseIdentifier
              match Equals
              val' <- parseValue
              return (name', typ, val')
          )
      match SemiColon
      return $ (name, typ, val) : rest
    _ -> return []

-- | Parse variable declarations
parseVarDecls :: Parser [(String, Type)]
parseVarDecls = do
  tok <- peek
  case tok of
    VAR -> do
      advance
      let parseOneDecl = do
            baseType <- parseType
            name <- parseIdentifier
            -- If it's a matrix type, parse the optional specifier here
            case baseType of
              MatrixType {} -> do
                mspec <- optional parseMatrixType
                match SemiColon
                return (name, baseType {matrixSpec = mspec})
              _ -> do
                match SemiColon
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
    PROCEDURE -> do
      advance
      name <- parseIdentifier
      match SemiColon
      block <- parseBlock
      match SemiColon
      return $ Procedure name block
    _ -> empty -- Use empty from Alternative instead of return []

-- | Parse a statement
parseInstruction :: Parser Statement
parseInstruction = do
  tok <- peek
  current <- gets currentToken
  case tok of
    BEGIN -> parseCompound
    CALL -> parseCall
    READ -> parseRead
    WRITE -> parseWrite
    IF -> parseIf
    WHILE -> parseWhile
    Identifier _ -> parseAssignment
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [BEGIN, CALL, READ, WRITE, IF, WHILE, Identifier ""],
            got = tok
          }

-- | Parse a compound statement
parseCompound :: Parser Statement
parseCompound = do
  match BEGIN
  stmts <- parseInstruction `sepBy1` match SemiColon
  match END
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
    INT8 -> advance >> return (IntType Int8)
    INT16 -> advance >> return (IntType Int16)
    INT32 -> advance >> return (IntType Int32)
    INT64 -> advance >> return (IntType Int64)
    INT128 -> advance >> return (IntType Int128)
    FLOAT8 -> advance >> return (FloatType Float8)
    FLOAT16 -> advance >> return (FloatType Float16)
    FLOAT32 -> advance >> return (FloatType Float32)
    FLOAT64 -> advance >> return (FloatType Float64)
    FLOAT128 -> advance >> return (FloatType Float128)
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [INT8, INT16, INT32, INT64, INT128, FLOAT8, FLOAT16, FLOAT32, FLOAT64, FLOAT128],
            got = tok
          }

  -- Check for matrix type
  matrixType <- optional $ do
    match LBracket
    -- Parse dimensions (e.g., [3,3])
    size1 <- parseNumber
    match Comma
    size2 <- parseNumber
    match RBracket
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
    -- TODO: inconsistent / Matrices are defined twice
    Token.Sparse -> advance >> return Ast.Sparse
    Token.Identity -> advance >> return Ast.Identity
    Token.Diagonal -> advance >> return Ast.Diagonal
    Token.UpperTriangular -> advance >> return Ast.UpperTriangular
    Token.LowerTriangular -> advance >> return Ast.LowerTriangular
    Token.Orthogonal -> advance >> return Ast.Orthogonal
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [Token.Sparse, Token.Identity, Token.Diagonal, Token.UpperTriangular, Token.LowerTriangular, Token.Orthogonal],
            got = tok
          }

-- | Parse a value (for constants)
parseValue :: Parser Value
parseValue = do
  tok <- peek
  current <- gets currentToken
  case tok of
    INumber n -> advance >> return (IntVal n)
    FNumber n -> advance >> return (FloatVal n)
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [INumber 0, FNumber 0.0],
            got = tok
          }

-- | Parse a number token
parseNumber :: Parser Integer
parseNumber = do
  tok <- peek
  current <- gets currentToken
  case tok of
    INumber n -> advance >> return n
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [INumber 0],
            got = tok
          }

-- | Parse an assignment statement
parseAssignment :: Parser Statement
parseAssignment = do 
  name <- parseIdentifier
  match Equals
  Assignment name <$> parseExpression

-- | Parse a call statement
parseCall :: Parser Statement
parseCall = do
  match CALL
  Call <$> parseIdentifier

-- | Parse a read statement
parseRead :: Parser Statement
parseRead = do
  match READ
  Read <$> parseIdentifier

-- | Parse a write statement
parseWrite :: Parser Statement
parseWrite = do
  match WRITE
  Write <$> parseExpression

-- | Parse an if statement
parseIf :: Parser Statement
parseIf = do
  match IF
  cond <- parseCondition
  match THEN
  If cond <$> parseInstruction

-- | Parse a while statement
parseWhile :: Parser Statement
parseWhile = do
  match WHILE
  cond <- parseCondition
  match DO
  While cond <$> parseInstruction

-- | Parse a condition
parseCondition :: Parser Condition
parseCondition = do
  tok <- peek
  case tok of
    NOT -> do
      advance
      Not <$> parseCondition
    _ -> do
      expr1 <- parseExpression
      op <- parseCompOp
      Compare expr1 op <$> parseExpression

-- | Parse a comparison operator
parseCompOp :: Parser CompOp
parseCompOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    Equals -> advance >> return Eq
    LessThen -> do
      advance
      hasEq <- optional $ match Equals
      return $ case hasEq of
        Just _ -> Lte
        Nothing -> Lt
    GreaterThen -> do
      advance
      hasEq <- optional $ match Equals
      return $ case hasEq of
        Just _ -> Gte
        Nothing -> Gt
    NotEqual -> advance >> return Neq
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [Equals, LessThen, GreaterThen, NotEqual],
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
    LParent -> do
      advance
      expr <- parseExpression
      match RParent
      return $ Factor $ Parens expr
    LBracket -> do
      -- Handle matrix literals
      advance
      match LBracket -- Expect second opening bracket
      -- Parse first row
      row1 <- parseMatrixRow
      match RBracket -- Close first row
      rows <- many $ do
        match Comma
        match LBracket
        row <- parseMatrixRow
        match RBracket
        return row
      match RBracket -- Close outer bracket
      return $ Factor $ MatrixLit (row1 : rows)
    INumber n -> advance >> return (Factor $ IntLit n)
    FNumber n -> advance >> return (Factor $ FloatLit n)
    Identifier name -> do
      advance
      -- Look ahead for possible matrix indexing
      tok' <- peek
      case tok' of
        LBracket -> do
          advance
          idx1 <- parseExpression
          match Comma
          idx2 <- parseExpression
          match RBracket
          return $ Factor $ MatrixIndex name (idx1, idx2)
        _ -> return $ Factor $ Var name
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [LParent, LBracket, INumber 0, FNumber 0.0, Identifier ""],
            got = tok
          }

-- | Parse a matrix row
parseMatrixRow :: Parser [Expression]
parseMatrixRow = do
  expr <- parseExpression
  rest <- many $ do
    match Comma
    parseExpression
  return (expr : rest)

-- | Parse an additive operator
parseAddOp :: Parser BinOp
parseAddOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    Plus -> advance >> return Add
    Minus -> advance >> return Sub
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [Plus, Minus],
            got = tok
          }

-- | Parse a multiplicative operator
parseMulOp :: Parser BinOp
parseMulOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    Times -> advance >> return Mul
    Divide -> advance >> return Div
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [Times, Divide],
            got = tok
          }

-- | Parse a matrix operator
parseMatrixOp :: Parser BinOp
parseMatrixOp = do
  tok <- peek
  current <- gets currentToken
  case tok of
    -- TODO: inconsistent / defined twice elementmult, elementdiv
    MatrixMult -> advance >> return MatrixMul
    Token.ElementMult -> advance >> return Ast.ElementMul
    Token.ElementDiv -> advance >> return Ast.ElementDiv
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [MatrixMult, Token.ElementMult, Token.ElementDiv],
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
    Plus | c == '+' -> advance >> return c
    Minus | c == '-' -> advance >> return c
    _ ->
      throwError $
        ParseError
          { errorPos = tokenOffset current,
            expected = [Plus | c == '+'] ++ [Minus | c == '-'],
            got = tok
          }
