module Lexer where

import Control.Applicative (Alternative (..), optional)
import Data.Char (isAlpha, isNumber, isSpace)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Token

-- define a newtype Lexer with kind i and a with a constructor Lexer with a function runLexer
-- i is our input for the lexer so in most cases a Char
-- a is our result type after we lexed somthing, would me a token in most cases
newtype Lexer i a = Lexer {runLexer :: [i] -> Offset -> Either [LexerError i] (Offset, a, [i])}

-- Divine how to apply a function to our lexer type
-- Lexer p where p has type [i] -> Either [LexerError i] (a, [i])
-- f :: a -> b
-- we return a new Lexer with with f applied if the lexer befor was sucsessfull otherwise return a Left with the error
instance Functor (Lexer i) where
  fmap f (Lexer p) = Lexer $ \input offset ->
    case p input offset of
      Left err -> Left err
      Right (offset', output, rest) -> Right (offset', f output, rest)

-- Define how we can combine a lexer with another lexer
-- if sucsessfull apllie the otherone else return the error
instance Applicative (Lexer i) where
  pure a = Lexer $ \input offset -> Right (offset, a, input)
  Lexer f <*> Lexer p = Lexer $ \input offset ->
    case f input offset of
      Left err -> Left err
      Right (offset', f', rest) ->
        case p rest offset' of
          Left err -> Left err
          Right (offset'', output, rest') -> Right (offset'', f' output, rest')

-- apply k to lexer with the lexeing of input was sucsessfully otherwise return the error
-- where k har the type a -> Lexer i b
instance Monad (Lexer i) where
  return = pure
  Lexer p >>= k = Lexer $ \input offset ->
    case p input offset of
      Left err -> Left err
      Right (offset', output, rest) ->
        let Lexer p' = k output
         in p' rest offset'

-- If the first lexer fails we use the alternative one otherwise we use the first one
instance (Eq i) => Alternative (Lexer i) where
  empty = Lexer $ \_ offset -> Left [LexerError offset Empty]

  Lexer l <|> Lexer r = Lexer $ \input offset ->
    case l input offset of
      Left err ->
        case r input offset of
          Left err' -> Left $ nub $ err <> err'
          Right (offset', output, rest) -> Right (offset', output, rest)
      Right (offset'', output, rest) -> Right (offset'', output, rest)

token' :: (i -> LexerErrorType i) -> (i -> Bool) -> Lexer i i
token' mkErr predicate = Lexer $ \input offset ->
  case input of
    [] -> Left [LexerError offset EndOfInput]
    hd : rest
      | predicate hd -> Right (offset + 1, hd, rest)
      | otherwise -> Left [LexerError offset $ mkErr hd]

satisfy :: (i -> Bool) -> Lexer i i
satisfy = token' Unexpected

char :: (Eq i) => i -> Lexer i i
char i = token' (Expected i) (== i)

string :: (Eq i) => [i] -> Lexer i [i]
string [] = return []
string (x : xs) = do
  y <- char x
  ys <- string xs
  return (y : ys)

eof :: Lexer i ()
eof = Lexer $ \input offset ->
  case input of
    [] -> Right (offset, (), [])
    hd : _ -> Left [LexerError offset $ ExpectedEndOfFile hd]

anyString :: Lexer Char String
anyString = some (satisfy isAlpha)

anyNumber :: Lexer Char String
anyNumber = some (satisfy isNumber)

number :: Lexer Char Token
number = do
  digits <- anyNumber
  hasDecimal <- optional (satisfy (== '.'))
  case hasDecimal of
    Nothing -> pure $ INumber (read digits)
    Just _ -> do
      digits' <- anyNumber
      pure $ FNumber (read $ digits ++ "." ++ digits')

identifier :: Lexer Char Token
identifier = do
  word <- anyString
  n <- optional anyNumber
  let ident = word ++ fromMaybe "" n
  case ident of
    "PROGRAMM" -> pure PROGRAMM
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
    _ -> pure $ Identifier word

symbols :: [Char] -> Lexer Char Char
symbols options = foldr1 (<|>) (map char options)

operator :: Lexer Char Token
operator = do
  symbol <- symbols ['=', '<', '>', '(', ')', '[', ']', '.', ';', '@', '\'', ',', ':']

  -- Cases for compound operators such
  case symbol of
    '.' -> do
      -- Look ahead for the next character
      next <- optional (symbols ['*', '/'])
      case next of
        Just '*' -> pure ElementMult
        Just '/' -> pure ElementDiv
        Nothing -> pure Dot
    '<' -> do
      -- Look ahead for =
      next <- optional (char '=')
      case next of
        Just _ -> pure LTE
        Nothing -> pure LessThen
    '>' -> do
      -- Look ahead for =
      next <- optional (char '=')
      case next of
        Just _ -> pure GTE
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
    _ -> Lexer $ \_ offset ->
      Left [LexerError offset $ Unexpected symbol]

whitespace :: Lexer Char ()
whitespace = do
  spaces <- some (satisfy isSpace)
  Lexer $ \input offset ->
    Right (offset, (), input)

token'' :: Lexer Char Token
token'' = identifier <|> number <|> operator

nextToken :: Lexer Char Token
nextToken = do
  _ <- optional whitespace
  token''

nextTokenWithPos :: Lexer Char TokenPos
nextTokenWithPos = do
  _ <- optional whitespace
  Lexer $ \input offset ->
    case runLexer token'' input offset of
      Left err -> Left err
      Right (offset', tok, rest) ->
        Right (offset', TokenPos offset tok, rest)

tokenize :: String -> Either [LexerError Char] (Offset, [TokenPos], [Char])
tokenize input = runLexer (many nextTokenWithPos <* eof) input 0

-- Missing / Additional Feature: comment support for the language
