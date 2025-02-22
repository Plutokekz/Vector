module SimpleParser where

import Ast
import Control.Applicative (Alternative (..), many, optional, some)
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Lexer (tokenize)
import Token (Token (..), TokenKeyword (..))

-- -----------------------------------------------------------------------------
-- 1. ParseError
-- -----------------------------------------------------------------------------

-- | Handle parsing errors with position information
data ParseError = ParseError
  -- TODO: eventually change Int to Line, Offset -> currently it's only the relative position from the start of the file
  { errorPos :: Int,
    expected :: [TokenKeyword],
    got :: TokenKeyword
  }
  deriving (Eq)

-- | Create a parse error from current token and expected tokens
mkError :: [TokenKeyword] -> Token -> ParseError
mkError expected token =
  ParseError
    { errorPos = tokenOffset token,
      expected = expected,
      got = tokenKeyword token
    }

-- | Show a parse error with context from the input string
showParseError :: String -> ParseError -> String
showParseError input (ParseError pos expected got) =
  unlines
    [ "Parse error at position "
        ++ show pos
        ++ ": Expected one from "
        ++ show expected
        ++ ", got "
        ++ show got,
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
data ParserState = ParserState
  { currentToken :: Token,
    remainingTokens :: [Token]
  }
  deriving (Show)

-- | The Parser monad - combines State for token management and Either for error handling
type Parser a = ExceptT ParseError (State ParserState) a

-- -----------------------------------------------------------------------------
-- 3. CORE PARSER OPERATIONS
-- -----------------------------------------------------------------------------

-- | Main parsing function that takes a list of tokens and returns either an error or AST
parse :: String -> Either String Program
parse input =
  case tokenize input of
    Left err -> Left (show err) -- Convert lexer error to string
    Right (_, tokens, _) ->
      case evalState (runExceptT parseProgram) (initParserState tokens) of
        Left err -> Left (showParseError input err)
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
    [] -> return (tokenKeyword current) -- At end of input
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

-- -----------------------------------------------------------------------------
-- 4. HELPER FUNCTIONS
-- -----------------------------------------------------------------------------

-- | Parse a sequence of elements separated by a delimiter (at least one element required)
parseSeparatedBy :: Parser a -> Parser sep -> Parser [a]
parseSeparatedBy element delimiter = do
  first <- element
  rest <- many (delimiter *> element)
  return (first : rest)

-- | Parse a token matching a predicate
parseToken :: (TokenKeyword -> Maybe a) -> [TokenKeyword] -> Parser a
parseToken predicate expected = do
  current <- gets currentToken
  case predicate (tokenKeyword current) of
    Just x -> advance >> return x
    Nothing -> throwError $ mkError expected current

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
  where
    getIdentifier (Identifier name) = Just name
    getIdentifier _ = Nothing

parseBlock :: Parser Block
parseBlock = do
  consts <- parseConstDecls
  vars <- parseVarDecls
  procs <- parseProcDecls
  Block consts vars procs <$> parseInstruction

parseConstDecls :: Parser [(String, Type, Value)]
parseConstDecls = do
  let parseOneConstDecl = do
        match CONST
        typ <- parseType
        decls <-
          parseSeparatedBy
            ( do
                name <- parseIdentifier
                match Equals
                val <- parseValue
                return (name, typ, val)
            )
            (match Comma)
        match SemiColon
        return decls
  concat <$> many parseOneConstDecl

parseVarDecls :: Parser [(String, Type)]
parseVarDecls = do
  current <- gets currentToken
  case tokenKeyword current of
    VAR -> do
      advance
      let parseOneDecl = do
            typ <- parseType
            name <- parseIdentifier
            match SemiColon
            return (name, typ)
      -- Parse one or more declarations using 'some'
      decls <- some parseOneDecl
      -- Look ahead to see if there's another VAR declaration
      nextDecls <- parseVarDecls
      return $ decls ++ nextDecls
    _ -> return []

parseType :: Parser Type
parseType = do
  -- First parse the base number type
  numType <- parseNumberType

  -- Check for dimensions and a specifier, corresponding to a vectorized type
  current <- gets currentToken
  case tokenKeyword current of
    Identifier "DIM" -> do
      advance
      match LParent
      dims <- parseNumber `parseSeparatedBy` match Comma
      match RParent
      spec <- optional parseSpecifier
      -- Convert single dimension to two dimensions [n] -> [1,n]
      let adjustedDims = case dims of
            [n] -> (1, n)  -- Convert vector dimension to 1×n matrix
            [w, h] -> (w, h)
      return $ VectorizedType numType adjustedDims spec
    -- If there are no brackets, it's a simple number type
    _ -> return $ NumberType numType

parseSpecifier :: Parser Specifier
parseSpecifier = parseToken getSpecifier specifierTokens
  where
    specifierTokens =
      [ Token.Sparse,
        Token.Identity,
        Token.Diagonal,
        Token.UpperTriangular,
        Token.LowerTriangular,
        Token.Orthogonal
      ]
    getSpecifier tok =
      case tok of
        Token.Sparse -> Just Ast.Sparse
        Token.Identity -> Just Ast.Identity
        Token.Diagonal -> Just Ast.Diagonal
        Token.UpperTriangular -> Just Ast.UpperTriangular
        Token.LowerTriangular -> Just Ast.LowerTriangular
        Token.Orthogonal -> Just Ast.Orthogonal
        _ -> Nothing

-- Helper to parse just the number type portion
parseNumberType :: Parser NumberType
parseNumberType = do
  current <- gets currentToken
  case tokenKeyword current of
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

parseProcDecls :: Parser [Procedure]
parseProcDecls =
  many $ do
    current <- gets currentToken
    case tokenKeyword current of
      PROCEDURE -> do
        advance
        name <- parseIdentifier
        match SemiColon
        block <- parseBlock
        match SemiColon
        return $ Procedure name block
      _ -> empty

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
  stmts <- parseInstruction `parseSeparatedBy` match SemiColon
  match END
  return $ Compound stmts

-- | Parse a value (for constants)
parseValue :: Parser Value
parseValue = parseSimpleValue <|> parseVectorizedValue

parseSimpleValue :: Parser Value
parseSimpleValue = parseToken getValue [INumber 0, FNumber 0.0]
  where
    getValue tok =
      case tok of
        INumber n -> Just $ IntVal n
        FNumber n -> Just $ FloatVal n
        _ -> Nothing

parseVectorizedValue :: Parser Value
parseVectorizedValue = do
  current <- gets currentToken
  case tokenKeyword current of
    LBracket -> do
      advance
      current' <- gets currentToken
      case tokenKeyword current' of
        LBracket -> parseMatrixValue -- [[...], [...]]
        _ -> parseVectorAsMatrixValue -- [...]
    _ -> throwError $ mkError [LBracket] current

-- | Parse a vector as a single-row matrix [1,2,3] -> [[1,2,3]]
parseVectorAsMatrixValue :: Parser Value
parseVectorAsMatrixValue = do
  elements <- parseValue `parseSeparatedBy` match Comma
  match RBracket
  return $ MatrixVal [elements]  -- Wrap in single row

-- | Parse a full matrix value
parseMatrixValue :: Parser Value
parseMatrixValue = do
  advance -- consume the second [
  row1 <- parseValue `parseSeparatedBy` match Comma
  match RBracket
  rows <- many $ do
    match Comma
    match LBracket
    row <- parseValue `parseSeparatedBy` match Comma
    match RBracket
    return row
  match RBracket
  return $ MatrixVal (row1 : rows)

-- | Parse a vector literal as a single-row matrix
parseVectorLiteral :: Parser Expression
parseVectorLiteral = do
  elements <- parseExpression `parseSeparatedBy` match Comma
  match RBracket
  return $ Factor $ VectorizedLit [elements]  -- Wrap in single row

-- | Parse a matrix literal
parseMatrixLiteral :: Parser Expression
parseMatrixLiteral = do
  advance -- consume the second [
  row1 <- parseExpression `parseSeparatedBy` match Comma
  match RBracket
  rows <- many $ do
    match Comma
    match LBracket
    row <- parseExpression `parseSeparatedBy` match Comma
    match RBracket
    return row
  match RBracket
  return $ Factor $ VectorizedLit (row1 : rows)

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
      op <- parseComparisonOp
      Compare expr1 op <$> parseExpression

-- | Parse a comparison operator
parseComparisonOp :: Parser CompOp
parseComparisonOp = parseToken getComparisonOp [IsEqual, LessThen, GreaterThen, LTE, GTE, NotEqual]
  where
    getComparisonOp tok =
      case tok of
        IsEqual -> Just Eq
        LessThen -> Just Lt
        GreaterThen -> Just Gt
        LTE -> Just Lte
        GTE -> Just Gte
        NotEqual -> Just Neq
        _ -> Nothing

-- | Parse a unary operator (+ or -) TODO eventually add transpose ' operator and evaluate if this is needed
parseUnaryOp :: Parser UnOp
parseUnaryOp = parseToken getUnaryOp [Plus, Minus]
  where
    getUnaryOp tok =
      case tok of
        Plus -> Just Pos
        Minus -> Just Neg
        _ -> Nothing

-- | Parse an expression
parseExpression :: Parser Expression
parseExpression = do
  sign <- optional parseUnaryOp
  term <- parseTerm
  let initial =
        case sign of
          Just Neg -> Unary Neg term
          _ -> term
  rest <-
    many $ do
      op <- parseAddOp
      term' <- parseTerm
      return (op, term')
  return $ foldl (\acc (op, t) -> Binary op acc t) initial rest

-- | Parse a term
parseTerm :: Parser Expression
parseTerm = do
  factor <- parseFactor
  rest <-
    many $ do
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
      -- Handle vector and matrix literals
      advance
      current' <- gets currentToken
      case tokenKeyword current' of
        LBracket -> parseMatrixLiteral -- If we see [[, it's a matrix
        _ -> parseVectorLiteral -- Otherwise it's a vector
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
          return $ Factor $ VectorizedIndex name (idx1, idx2)
        _ -> return $ Factor $ Var name
    _ ->
      throwError $
        mkError
          [LParent, LBracket, INumber 0, FNumber 0.0, Identifier ""]
          current

-- | Parse an additive operator
parseAddOp :: Parser BinOp
parseAddOp = parseToken getAddOp [Plus, Minus]
  where
    getAddOp tok =
      case tok of
        Plus -> Just Add
        Minus -> Just Sub
        _ -> Nothing

-- | Parse a multiplicative operator
parseMulOp :: Parser BinOp
parseMulOp = parseToken getMulOp [Times, Divide]
  where
    getMulOp tok =
      case tok of
        Times -> Just Mul
        Divide -> Just Div
        _ -> Nothing

-- | Parse a matrix operator
parseMatrixOp :: Parser BinOp
parseMatrixOp =
  parseToken getOp [Token.ElementMult, Token.ElementDiv]
  where
    getOp tok =
      case tok of
        Token.ElementMult -> Just Ast.ElementMul
        Token.ElementDiv -> Just Ast.ElementDiv
        _ -> Nothing
