module SimpleParser
where

import Control.Applicative (Alternative (..), many, optional)
import Control.Monad.Except
import Control.Monad.State
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Token (TokenKeyword(..), Token(..))
import Ast
import Lexer (tokenize)

-- -----------------------------------------------------------------------------
-- 1. ParseError 
-- -----------------------------------------------------------------------------

-- | Handle parsing errors with position information
data ParseError = ParseError { 
  -- TODO: eventually change Int to Line, Offset -> currently it's only the relative position from the start of the file
  errorPos :: Int,
  expected :: [TokenKeyword],
  got :: TokenKeyword
} deriving (Eq)

-- | Create a parse error from current token and expected tokens
mkError :: [TokenKeyword] -> Token -> ParseError
mkError expected token = ParseError {
  errorPos = tokenOffset token,
  expected = expected,
  got = tokenKeyword token
}

-- | Show a parse error with context from the input string
showParseError :: String -> ParseError -> String
showParseError input (ParseError pos expected got) =
  unlines [
    "Parse error at position " ++ show pos ++ ": Expected one from " ++ show expected ++ ", got " ++ show got,
    "Near: " ++ take pos input ++ "<ERROR LOCATION>"
  ]

-- | Used to define the mappend operation
instance Semigroup ParseError where
  e1 <> _ = e1 -- Only keep the first error

-- | Needed for many and some
instance Monoid ParseError where
  mempty = ParseError 0 [] (Identifier "")
  mappend = (<>)

-- -----------------------------------------------------------------------------
-- 2. ParserState
-- -----------------------------------------------------------------------------

-- | State of the parser containing current and remaining tokens (each token includes its position)
data ParserState = ParserState { 
  currentToken :: Token,
  remainingTokens :: [Token]
} deriving (Show)

-- | The Parser monad - combines State for token management and Either for error handling
type Parser a = ExceptT ParseError (State ParserState) a

-- -----------------------------------------------------------------------------
-- 3. CORE PARSER OPERATIONS
-- -----------------------------------------------------------------------------

-- | Main parsing function that takes a list of tokens and returns either an error or AST
parse :: String -> Either String Program
parse input = case tokenize input of
  Left err -> Left (show err)  -- Convert lexer error to string
  Right (_, tokens, _) -> case evalState (runExceptT parseProgram) (initParserState tokens) of
    Left err  -> Left (showParseError input err)
    Right ast -> Right ast

-- | Initialize parser state with a list of tokens
initParserState :: [Token] -> ParserState
initParserState [] = error "Cannot parse empty token stream."
initParserState (t : ts) = ParserState t ts

-- | Consume and return the current token, advancing to the next one
advance :: Parser TokenKeyword
advance = do
  current <- gets currentToken
  remaining <- gets remainingTokens
  case remaining of
    []            -> return (tokenKeyword current) -- At end of input
    (next : rest) -> do
      put (ParserState next rest)
      return (tokenKeyword current)

-- | Match a specific token, consuming it if it matches
match :: TokenKeyword -> Parser TokenKeyword
match expected = do
  current <- gets currentToken
  if tokenKeyword current == expected 
    then advance 
    else throwError $ mkError [expected] current

-- | Parse a token matching a predicate
parseToken :: (TokenKeyword -> Maybe a) -> [TokenKeyword] -> Parser a
parseToken predicate expected = do
  current <- gets currentToken
  case predicate (tokenKeyword current) of
    Just x -> advance >> return x
    Nothing -> throwError $ mkError expected current

-- -----------------------------------------------------------------------------
-- 4. HELPER FUNCTIONS
-- -----------------------------------------------------------------------------

-- Helper function for parsing separated by
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  return (x : xs)

-- Helper for parseMany
parseMany :: Parser a -> Parser [a]
parseMany p = do
  x <- p
  xs <- many p
  return (x : xs)

-- Helper for char
char :: Char -> Parser Char
char c = do
  current <- gets currentToken
  case tokenKeyword current of
    Plus | c == '+' -> advance >> return c
    Minus | c == '-' -> advance >> return c
    _ -> throwError $ mkError expectedTokens current
      where expectedTokens = [Plus | c == '+'] ++ [Minus | c == '-']

-- -----------------------------------------------------------------------------
-- 5. CONCRETE PARSERS
-- -----------------------------------------------------------------------------

parseProgram :: Parser Program
parseProgram = do
  match PROGRAM
  name <- parseIdentifier
  match Colon
  block <- parseBlock
  match Dot
  return (Program name block)

parseIdentifier :: Parser String
parseIdentifier = parseToken getIdentifier [Identifier ""]
  where getIdentifier (Identifier name) = Just name
        getIdentifier _ = Nothing

parseBlock :: Parser Block
parseBlock = do
  consts <- parseConstDecls
  vars <- parseVarDecls
  procs <- parseProcDecls
  instruction <- parseInstruction
  return $ Block consts vars procs instruction

parseConstDecls :: Parser [(String, Type, Value)]
parseConstDecls = do
  current <- gets currentToken
  case tokenKeyword current of
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

parseVarDecls :: Parser [(String, Type)]
parseVarDecls = do
  current <- gets currentToken
  case tokenKeyword current of
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

parseProcDecls :: Parser [Procedure]
parseProcDecls = many $ do
  current <- gets currentToken
  case tokenKeyword current of
    PROCEDURE -> do
      advance
      name <- parseIdentifier
      match SemiColon
      block <- parseBlock
      match SemiColon
      return $ Procedure name block
    _ -> empty -- Use empty from Alternative instead of return []

parseInstruction :: Parser Statement
parseInstruction = do
  current <- gets currentToken
  case tokenKeyword current of
    BEGIN -> parseCompoundInstruction
    CALL -> parseCall
    READ -> parseRead
    WRITE -> parseWrite
    IF -> parseIf
    WHILE -> parseWhile
    Identifier _ -> parseAssignment
    _ -> throwError $ mkError [BEGIN, CALL, READ, WRITE, IF, WHILE, Identifier ""] current

parseCompoundInstruction :: Parser Statement
parseCompoundInstruction = do
  match BEGIN
  stmts <- parseInstruction `sepBy1` match SemiColon
  match END
  return $ Compound stmts

parseType :: Parser Type
parseType = do
  current <- gets currentToken
  baseType <- case tokenKeyword current of
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
    _ -> throwError $ mkError [INT8, INT16, INT32, INT64, INT128, FLOAT8, FLOAT16, FLOAT32, FLOAT64, FLOAT128] current

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

parseMatrixType :: Parser MatrixSpecifier
parseMatrixType = parseToken getSpecifier specifierTokens
  where
    specifierTokens = [Token.Sparse, Token.Identity, Token.Diagonal, 
                      Token.UpperTriangular, Token.LowerTriangular, Token.Orthogonal]
    
    getSpecifier tok = case tok of
      Token.Sparse -> Just Ast.Sparse
      Token.Identity -> Just Ast.Identity
      Token.Diagonal -> Just Ast.Diagonal
      Token.UpperTriangular -> Just Ast.UpperTriangular
      Token.LowerTriangular -> Just Ast.LowerTriangular
      Token.Orthogonal -> Just Ast.Orthogonal
      _ -> Nothing

-- | Parse a value (for constants)
parseValue :: Parser Value
parseValue = parseToken getValue [INumber 0, FNumber 0.0]
  where
    getValue tok = case tok of
      INumber n -> Just $ IntVal n
      FNumber n -> Just $ FloatVal n
      _ -> Nothing

-- | Parse a number token
parseNumber :: Parser Integer
parseNumber = parseToken getNumber [INumber 0]
  where
    getNumber (INumber n) = Just n
    getNumber _ = Nothing

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
  current <- gets currentToken
  case tokenKeyword current of
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
  current <- gets currentToken
  case tokenKeyword current of
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
    _ -> throwError $ mkError [Equals, LessThen, GreaterThen, NotEqual] current

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
  current <- gets currentToken
  case tokenKeyword current of
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
      current' <- gets currentToken
      case tokenKeyword current' of
        LBracket -> do
          advance
          idx1 <- parseExpression
          match Comma
          idx2 <- parseExpression
          match RBracket
          return $ Factor $ MatrixIndex name (idx1, idx2)
        _ -> return $ Factor $ Var name
    _ -> throwError $ mkError [LParent, LBracket, INumber 0, FNumber 0.0, Identifier ""] current

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
parseAddOp = parseToken getAddOp [Plus, Minus]
  where
    getAddOp tok = case tok of
      Plus -> Just Add
      Minus -> Just Sub
      _ -> Nothing

-- | Parse a multiplicative operator
parseMulOp :: Parser BinOp
parseMulOp = parseToken getMulOp [Times, Divide]
  where
    getMulOp tok = case tok of
      Times -> Just Mul
      Divide -> Just Div
      _ -> Nothing

-- | Parse a matrix operator
parseMatrixOp :: Parser BinOp
parseMatrixOp = parseToken getOp [MatrixMult, Token.ElementMult, Token.ElementDiv]
  where
    getOp tok = case tok of
      MatrixMult -> Just MatrixMul
      Token.ElementMult -> Just Ast.ElementMul
      Token.ElementDiv -> Just Ast.ElementDiv
      _ -> Nothing
