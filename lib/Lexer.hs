module Lexer (tokenize) where

import Control.Applicative (Alternative (..), optional)
import Data.Char (isAlpha, isNumber, isSpace)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Token (Token(..), TokenKeyword(..), Offset)


-- LexerError and Error Types
data LexerError i = LexerError {
  errorOffset :: Offset, 
  error :: LexerErrorType i
} deriving (Eq)

instance Show i => Show (LexerError i) where
  show (LexerError offset err) = "Lexer error at position " ++ show offset ++ ": " ++ show err

-- Error Types take an input to return the character(s) that caused the error
data LexerErrorType i
  = EndOfInput
  | Empty
  | Unexpected i
  | Expected i i
  | ExpectedUnicodeAlphabetic i
  | ExpectedNumeric i
  | ExpectedEndOfFile i
  | ExpectedSpace i
  | UnexpectedNewline i
  deriving (Eq)

instance Show i => Show (LexerErrorType i) where
  show EndOfInput = "End of input"
  show Empty = "Empty"
  show (Unexpected i) = "Unexpected character: " ++ show i
  show (Expected expected got) = "Expected character: " ++ show expected ++ ", got: " ++ show got
  show (ExpectedUnicodeAlphabetic i) = "Expected unicode alphabetic character, got: " ++ show i
  show (ExpectedNumeric i) = "Expected numeric character, got: " ++ show i
  show (ExpectedEndOfFile i) = "Expected end of file, got: " ++ show i
  show (ExpectedSpace i) = "Expected space, got: " ++ show i
  show (UnexpectedNewline i) = "Unexpected newline, got: " ++ show i

-- Lexer
-- i: input for the lexer, in most cases a Char
-- a: result after lexing, in most cases a Token Keyword
newtype Lexer i a = Lexer {
  runLexer :: [i] -> Offset -> Either [LexerError i] (Offset, a, [i])
}


-- Lexer Functor, Applicative, Monad and Alternative instances
-- Return a new Lexer with with f applied to output previous Lexer is succesful, otherwise return a Left with the error
instance Functor (Lexer i) where
  fmap :: (a -> b) -> Lexer i a -> Lexer i b
  fmap f (Lexer p) = Lexer $ \input offset -> case p input offset of
    Left err                      -> Left err
    Right (offset', output, rest) -> Right (offset', f output, rest)

-- Define how Lexers can be combined
-- if lexing is succesful, apply the other one, otherwise return the error
instance Applicative (Lexer i) where
  pure :: a -> Lexer i a
  pure a = Lexer $ \input offset -> Right (offset, a, input)

  (<*>) :: Lexer i (a -> b) -> Lexer i a -> Lexer i b
  Lexer g <*> Lexer p = Lexer $ \input offset -> case g input offset of
    Left err                  -> Left err
    Right (offset', g', rest) -> case p rest offset' of
      Left err                        -> Left err
      Right (offset'', output, rest') -> Right (offset'', g' output, rest')

instance Monad (Lexer i) where
  (>>=) :: Lexer i a -> (a -> Lexer i b) -> Lexer i b
  Lexer p >>= k = Lexer $ \input offset -> case p input offset of
    Left err                      -> Left err
    Right (offset', output, rest) -> let Lexer p' = k output in p' rest offset'

-- If the first lexer fails we use the alternative one otherwise we use the first one
instance (Eq i) => Alternative (Lexer i) where
  empty :: Eq i => Lexer i a
  empty = Lexer $ \_ offset -> Left [LexerError offset Empty]

  (<|>) :: Eq i => Lexer i a -> Lexer i a -> Lexer i a
  Lexer l <|> Lexer r = Lexer $ \input offset -> case l input offset of 
    Left err                       -> case r input offset of 
      Left err'                     -> Left $ nub $ err <> err'
      Right (offset', output, rest) -> Right (offset', output, rest)
    Right (offset'', output, rest) -> Right (offset'', output, rest)


-- Helper function to return a single character if the predicate is true for the input, otherwise return an error
matchInputWithError :: (i -> LexerErrorType i) -> (i -> Bool) -> Lexer i i
matchInputWithError raiseError predicate = Lexer $ \input offset -> case input of
    [] -> Left [LexerError offset EndOfInput]
    firstChar : rest
      | predicate firstChar -> Right (offset + 1, firstChar, rest) 
      | otherwise -> Left [LexerError offset $ raiseError firstChar]


-- Predicate matching functions for different error types
satisfyAlpha :: Lexer Char Char
satisfyAlpha = matchInputWithError ExpectedUnicodeAlphabetic isAlpha

satisfyNumeric :: Lexer Char Char
satisfyNumeric = matchInputWithError ExpectedNumeric isNumber

satisfySpace :: Lexer Char Char
satisfySpace = matchInputWithError ExpectedSpace isSpace

satisfyNotNewline :: Lexer Char Char
satisfyNotNewline = matchInputWithError UnexpectedNewline (/= '\n')


-- Functions to match specific characters, sprints or EOF
matchChar :: (Eq i) => i -> Lexer i i
matchChar i = matchInputWithError (Expected i) (== i)

matchString :: (Eq i) => [i] -> Lexer i [i]
matchString [] = return []
matchString (x : xs) = do
  y  <- matchChar x
  ys <- matchString xs
  return (y : ys)

matchEOF :: Lexer i ()
matchEOF = Lexer $ \input offset -> case input of
  []     -> Right (offset, (), [])
  firstChar : _ -> Left [LexerError offset $ ExpectedEndOfFile firstChar]


-- Actual lexable objects: number, identifier, operator, comment, whitespaces 

-- TODO: number currently only differentiates between Integers (INumber) and Floats (FNumber), no precisions
 -- could easily add precision by counting the length of the number here
number :: Lexer Char TokenKeyword
number = do
  digits <- some satisfyNumeric
  hasDecimal <- optional (matchChar '.')
  case hasDecimal of
    Nothing -> return $ INumber (read digits)
    Just _  -> do
      decimalDigits <- some satisfyNumeric
      return $ FNumber (read $ digits ++ "." ++ decimalDigits)

identifier :: Lexer Char TokenKeyword
identifier = do
  word <- some satisfyAlpha
  num <- optional (some satisfyNumeric)
  let ident = word ++ fromMaybe "" num
  case ident of
    "PROGRAM" -> pure PROGRAM
    "CONST" -> pure CONST
    "VAR" -> pure VAR
    "PROCEDURE" -> pure PROCEDURE
    "CALL" -> pure CALL
    "READ" -> pure READ
    "WRITE" -> pure WRITE
    "BEGIN" -> pure BEGIN
    "IF" -> pure IF
    "THEN" -> pure THEN
    "WHILE" -> pure WHILE
    "DO" -> pure DO
    "NOT" -> pure NOT
    "INT8" -> pure INT8
    "INT16" -> pure INT16
    "INT32" -> pure INT32
    "INT64" -> pure INT64
    "INT128" -> pure INT128
    "FLOAT8" -> pure FLOAT8
    "FLOAT16" -> pure FLOAT16
    "FLOAT32" -> pure FLOAT32
    "FLOAT64" -> pure FLOAT64
    "FLOAT128" -> pure FLOAT128
    "Sparse" -> pure Sparse
    "Identity" -> pure Identity
    "Diagonal" -> pure Diagonal
    "Orthogonal" -> pure Orthogonal
    "LowerTriangular" -> pure LowerTriangular
    "END" -> pure END
    _ -> pure (Identifier word)

symbols :: [Char] -> Lexer Char Char
symbols options = foldr1 (<|>) (map matchChar options)

operator :: Lexer Char TokenKeyword
operator = do
  symbol <- symbols ['=', '<', '>', '(', ')', '[', ']', '.', ';', '@', '\'', ',', ':', '+', '-', '*', '/']

  -- Cases for compound operators such
  case symbol of
    '.' -> do
      -- Look ahead for the next character
      next <- optional (symbols ['*', '/'])
      case next of
        Just '*' -> pure ElementMult
        Just '/' -> pure ElementDiv
        Nothing  -> pure Dot
    '<' -> do
      -- Look ahead for =
      next <- optional (matchChar '=')
      case next of
        Just _  -> pure LTE
        Nothing -> pure LessThen
    '>' -> do
      -- Look ahead for =
      next <- optional (matchChar '=')
      case next of
        Just _  -> pure GTE
        Nothing -> pure GreaterThen

    -- simple operators
    '=' -> pure Equals
    '(' -> pure LParent
    ')' -> pure RParent
    ',' -> pure Comma
    '[' -> pure LBracket
    ']' -> pure RBracket
    ';' -> pure SemiColon
    ':' -> pure Colon
    '@' -> pure MatrixMult
    '\'' -> pure Transpose
    '+' -> pure Plus
    '-' -> pure Minus
    '*' -> pure Times
    '/' -> pure Divide
    _ -> Lexer $ \_ offset ->
      Left [LexerError offset $ Unexpected symbol]

-- Handle comments
comment :: Lexer Char ()
comment = do
    matchString "--"  -- Match comment start
    many satisfyNotNewline  -- Match everything until newline
    -- optional (matchChar '\n')  -- Match optional newline
    return ()

-- Update whitespace to handle comments
whitespace :: Lexer Char ()
whitespace = do
    some (satisfySpace <|> (do 
        comment
        return ' '))
    return ()

lexToken :: Lexer Char TokenKeyword
lexToken = identifier <|> number <|> operator

nextToken :: Lexer Char TokenKeyword
nextToken = do
  optional whitespace
  lexToken

nextTokenWithPos :: Lexer Char Token
nextTokenWithPos = do
  optional whitespace
  Lexer $ \input offset -> case runLexer lexToken input offset of
      Left err                   -> Left err
      Right (offset', tok, rest) -> Right (offset', Token offset tok, rest)

tokenize :: String -> Either [LexerError Char] (Offset, [Token], [Char])
tokenize input = runLexer tokenizeSequence input 0
  where
    tokenizeSequence = do
      tokens <- many nextTokenWithPos      -- Parse all tokens
      optional whitespace                  -- Skip any trailing whitespace
      matchEOF                            -- Ensure we're at end of input
      return tokens
