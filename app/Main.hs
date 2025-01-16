module Main where

import Lexer (tokenize)

main :: IO ()
main = do
  let x = tokenize "PROGRAM test BEGIN x = 42.5 END."
  print x
  return ()
