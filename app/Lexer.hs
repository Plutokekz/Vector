module Lexer where

import Control.Applicative (Alternative (..))
import Data.List (nub)

type Offset = Int

data LexerError i = LexerError {erOffset :: Offset, erError :: LexerErrorType i} deriving (Eq, Show)

data LexerErrorType i
  = EndOfInput
  | Unexpected i
  | Empty
  | ExpectedEndOfFile i
  | Expected i i
  deriving (Eq, Show)

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

token :: (i -> LexerErrorType i) -> (i -> Bool) -> Lexer i i
token mkErr predicate = Lexer $ \input offset ->
  case input of
    [] -> Left [LexerError offset EndOfInput]
    hd : rest
      | predicate hd -> Right (offset + 1, hd, rest)
      | otherwise -> Left [LexerError offset $ mkErr hd]

satisfy :: (i -> Bool) -> Lexer i i
satisfy = token Unexpected


char :: Eq i => i -> Lexer i i
char i = token (Expected i) (== i)

string :: Eq i => [i] -> Lexer i [i]
string [] = return []
string (x : xs) = do
  y <- char x
  ys <- string xs
  return (y : ys)

eof :: Lexer i ()
eof = Lexer $ \input offset ->
  case input of
    [] -> Right (0, (), [])
    hd : _ -> Left [LexerError offset $ ExpectedEndOfFile hd]